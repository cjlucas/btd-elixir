defmodule FileStream.Manager do
  use GenServer

  @type offset :: integer
  @type size :: integer

  @type segment :: {String.t, offset, size}

  defmodule File do
    defstruct name: nil, offset: 0, size: 0
  end

  defmodule State do
    defstruct root: nil, piece_size: 0, files: []
  end

  def start_link(root, piece_size, files) do
    GenServer.start_link(__MODULE__, {root, piece_size, files}, [])
  end

  def init({root, piece_size, files}) do
    # calc offset for each file
    files = Enum.scan(files, %File{}, fn f, acc ->
      Map.merge(%{offset: acc.offset + acc.size}, f)
    end)

    {:ok, %State{root: root, piece_size: piece_size, files: files}}
  end

  def read_block(pid, piece_idx, offset, length) do
    GenServer.call(pid, {:read_block, piece_idx, offset, length})
  end

  def write_block(pid, piece_idx, offset, data) do
    GenServer.cast(pid, {:write_block, piece_idx, offset, data})
  end

  defp chunk(sizes, buf), do: chunk(sizes, buf, [])
  defp chunk([], _, acc), do: Enum.reverse(acc)
  defp chunk([n | sizes], buf, acc) do
    << buf :: bytes-size(n), rest :: binary >> = buf
    chunk(sizes, rest, [buf | acc])
  end

  defp write(fpath, off, data) do
    with {:ok, f} = :file.open(fpath, [:read, :write, :binary, :raw]),
         :ok = :file.pwrite(f, off, data),
         :ok = :file.close(f),
      do: :ok
  end

  defp read(fpath, off, len) do
    with {:ok, f} = :file.open(fpath, [:read, :binary, :raw]),
         {:ok, data} = :file.pread(f, off, len),
         :ok = :file.close(f),
      do: data
  end

  def handle_call({:read_block, piece_idx, offset, length}, _from, state) do
    offset = piece_idx * state.piece_size + offset
    data = Bittorrent.FileStream.items_in_range(state.files, offset, length)
           |> Enum.map(fn item ->
             read(Path.join(state.root, item.name), item.offset, item.size)
           end)
           |> Enum.reduce(fn buf, acc -> acc <> buf end)

    {:reply, data, state}
  end

  def handle_cast({:write_block, piece_idx, offset, data}, state) do
    offset = piece_idx * state.piece_size + offset
    segments = Bittorrent.FileStream.items_in_range(state.files, offset, byte_size(data))

    segments
    |> Enum.map(&(&1.size))
    |> chunk(data)
    |> Enum.zip(segments)
    |> Enum.map(fn t -> Map.merge(elem(t, 1), %{data: elem(t, 0)}) end)
    |> Enum.each(fn seg -> write(Path.join(state.root, seg.name), seg.offset, seg.data) end)

    {:noreply, state}
  end
end
