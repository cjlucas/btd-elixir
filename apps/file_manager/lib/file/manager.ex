defmodule File.Manager do

  @moduledoc """
  Manages file set for a given torrent
  """

  use GenServer

  @block_size 16384

  @type offset :: integer
  @type size :: integer

  @type file :: {String.t, size}
  @type segment :: {String.t, offset, size}

  defmodule Block do
    defstruct offset: 0, size: 0, segments: [], status: :need
  end

  defmodule State do
    defstruct root: "", piece_hashes: [], blocks: %{}
  end

  @spec start_link(binary, String.t, [file], [binary], integer) :: {:ok, pid}
  def start_link(info_hash, root, files, piece_hashes, piece_size) do
    name = via(info_hash)
    GenServer.start_link(__MODULE__, {root, files, piece_hashes, piece_size}, name: name)
  end
  
  def write_block(info_hash, piece_idx, offset, data) do
    via(info_hash) |> GenServer.call({:write_block, piece_idx, offset, data})
  end

  def init({root, files, piece_hashes, piece_size}) do
    blocks = 
      Enum.reduce(files, 0, fn {_, size}, acc -> acc + size end) # calc total size
      |> chunk(piece_size) # chunk pieces into list of block sizes
      |> Enum.map(fn size -> # map to blocks
        chunk(size, @block_size)
        |> Enum.map_reduce(0, fn bs, offset ->
          block = %Block{offset: offset, size: bs}
          segs = segments(block, files)
          {%{block | segments: segs}, bs + offset}
        end)
        |> elem(0) # pick list of blocks
      end)
      |> Enum.reduce({%{}, 0}, fn block, {m, i} -> # reduce to map
        {Map.put(m, i, block), i+1}
      end)
      |> elem(0)

      IO.puts(inspect blocks)
    {:ok, %State{root: root, piece_hashes: piece_hashes, blocks: blocks}}
  end

  def handle_call({:write_block, piece_idx, offset, data}, _from, %{root: root, blocks: blocks} = state) do
    %{segments: segments} =
      Map.get(blocks, piece_idx)
      |> Enum.filter(fn %{offset: off, size: size} -> 
        offset == off && byte_size(data) == size
      end)
      |> List.first

    IO.puts(inspect segments)
    
    results = segments
      |> Enum.map(fn {fname, offset, size} -> 
        <<_::bytes-size(offset), seg_data::bytes-size(size), _::binary>> = data
        Path.join(root, fname)
        |> Torrent.FileHandler.Manager.write(offset, seg_data)
      end)

    reply = case Enum.filter(results, fn res -> res == :ok end) do
      [] -> :ok
      l  -> List.first(l)
    end

    # TODO: block needs to be marked as written
    {:reply, reply, state}
  end

  @spec segments(Block.t, [file]) :: [segment]
  def segments(%{offset: offset, size: size}, files) do
    files =
      files
      |> Enum.map_reduce(0, fn {fname, size}, offset -> 
        {{fname, offset, size}, offset + size}
      end)
      |> elem(0)
      |> Enum.filter(fn {_, off, _} -> offset >= off || offset + size >= off end)


    segments(offset - (List.first(files) |> elem(1)), size, files, [])
  end
  defp segments(_offset, bytes_left, _files, acc) when bytes_left == 0 do
    Enum.reverse(acc)
  end
  defp segments(offset, bytes_left, [{fname, _, size} | rest], acc) do
    seg_size = Enum.min([bytes_left, size - offset])
    seg = {fname, offset, seg_size} 
    segments(0, bytes_left - seg_size, rest, [seg | acc])
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

  defp via(info_hash) do
    {:via, Registry, {File.Manager.Registry, info_hash}}
  end
end
