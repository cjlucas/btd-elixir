defmodule File.Manager do

  @moduledoc """
  Manages file set for a given torrent
  """

  use GenServer

  @type file :: {String.t, integer}
  @type offset :: integer
  @type size :: integer

  @type segment :: {String.t, offset, size}


  defmodule Block do
    @type status :: :need | :written | :verified

    defstruct offset: 0, size: 0, status: :need
  end

  defmodule State do
    @block_size 16384

    # FIXME
    @type t :: String.t

    defstruct root: "", files: [], piece_size: 0, piece_hashes: [], blocks: %{}

    def new(root, files, piece_hashes, piece_size) do
      blocks = 
        Enum.reduce(files, 0, fn {_, size}, offset -> offset + size end) # calc total size
        |> chunk(piece_size) # chunk pieces into list of block sizes
        |> Enum.map(&chunk(&1, @block_size))
        |> Enum.map(fn blocks ->
          Enum.map_reduce(blocks, 0, fn block_size, offset ->
            {%Block{offset: offset, size: block_size}, block_size + offset}
          end) |> elem(0)
        end)
        |> Enum.reduce({%{}, 0}, fn block, {m, i} -> # reduce to map
          {Map.put(m, i, block), i+1}
        end)
        |> elem(0)

      %State{
        root: root,
        files: files,
        piece_size: piece_size,
        piece_hashes: piece_hashes,
        blocks: blocks
      }
    end

    def update_block(%{blocks: blocks} = state, piece_idx, block_offset, block_size, status) do
      block_idx = Map.get(blocks, piece_idx)
                  |> Enum.find_index(fn %{offset: off, size: size} -> 
                    block_offset == off && block_size == size
                  end)

      if is_nil(block_idx) do
        raise ArgumentError, "block not found"
      end

      blk_lst = Map.get(blocks, piece_idx)
                |> List.update_at(block_idx, &(%{&1 | status: status}))

      %{state | blocks: Map.put(blocks, piece_idx, blk_lst)}
    end

    def find_block(%{blocks: blocks}, piece_idx, block_offset, block_size) do
      Map.get(blocks, piece_idx)
      |> Enum.filter(fn %{offset: off, size: size} -> 
        block_offset == off && block_size == size
      end)
      |> List.first
    end

    def chunk(total, chunk_size) when total < chunk_size do
      [total]
    end
    def chunk(total, chunk_size) when rem(total, chunk_size) > 0 do
      r = rem(total, chunk_size)
      chunk(total - r, chunk_size) ++ [r]
    end
    def chunk(total, chunk_size) do
      full_pieces = div(total, chunk_size)
      Enum.map(1..full_pieces, fn _ -> chunk_size end)
    end
  end

  @spec start_link(binary, String.t, [file], [binary], integer) :: {:ok, pid}
  def start_link(info_hash, root, files, piece_hashes, piece_size) do
    name = via(info_hash)
    GenServer.start_link(__MODULE__, {root, files, piece_hashes, piece_size}, name: name)
  end

  def write_block(info_hash, piece_idx, offset, data) do
    via(info_hash) |> GenServer.call({:write_block, piece_idx, offset, data})
  end

  def read_block(info_hash, piece_idx, offset, length) do
    via(info_hash) |> GenServer.call({:read_block, piece_idx, offset, length})
  end

  def init({root, files, piece_hashes, piece_size}) do
    {:ok, State.new(root, files, piece_hashes, piece_size)}
  end

  def handle_call({:write_block, piece_idx, offset, data}, _from, state) do
    results =
      segments(state, piece_idx, offset, byte_size(data))
      |> Enum.map_reduce(0, fn {fpath, offset, size}, block_offset ->
        {{fpath, offset, size, block_offset}, size + block_offset} 
      end)
      |> elem(0)
      |> Enum.map(fn {fpath, offset, size, block_offset} -> 
        <<_::bytes-size(block_offset), seg_data::bytes-size(size), _::binary>> = data
        Torrent.FileHandler.Manager.write(fpath, offset, seg_data)
      end)

    reply = case Enum.filter(results, &(&1 == :ok)) do
      [] -> :ok
      l  -> List.first(l)
    end

    state = case reply do
      :ok         -> State.update_block(state, piece_idx, offset, byte_size(data), :written)
      {:error, _} -> state
    end

    {:reply, reply, state}
  end

  def handle_call({:read_block, piece_idx, offset, length}, _from, %{blocks: blocks} = state) do
  end

  def terminate(_reason, %{root: root, files: files}) do
    files
    |> Enum.map(&elem(&1, 0))
    |> Enum.map(fn fname -> Path.join(root, fname) end)
    |> Enum.each(&(Torrent.FileHandler.Manager.close(&1)))
  end

  @spec segments(State.t, offset, size, [file]) :: [segment]
  def segments(%{root: root, piece_size: piece_size, files: files}, piece_idx, offset, size) do
    offset = (piece_idx * piece_size) + offset

    files =
      files
      |> Enum.map_reduce(0, fn {fname, size}, offset -> 
        {{fname, offset, size}, offset + size}
      end)
      |> elem(0)
      |> Enum.filter(fn {_, off, fsize} -> offset <= off + fsize end)

    calc_segments(offset - (List.first(files) |> elem(1)), size, files, [])
    |> Enum.map(fn {fname, offset, size} ->
      {Path.join(root, fname), offset, size}
    end)
  end

  defp calc_segments(_offset, bytes_left, _files, acc) when bytes_left == 0 do
    Enum.reverse(acc)
  end
  defp calc_segments(offset, bytes_left, [{fname, _, size} | rest], acc) do
    seg_size = Enum.min([bytes_left, size - offset])
    seg = {fname, offset, seg_size} 
    calc_segments(0, bytes_left - seg_size, rest, [seg | acc])
  end

  defp via(info_hash) do
    {:via, Registry, {File.Manager.Registry, info_hash}}
  end
end
