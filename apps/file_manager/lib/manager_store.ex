defmodule File.Manager.Store do

  @type status :: :need | :have | :verified

  @type file :: {String.t, size :: integer}

  # offset is the offset within the piece
  @type block :: {offset :: integer, size :: integer, status}
  
  @type segment :: {String.t, offset :: integer, size :: integer}

  defmodule Entry do
    @type file :: {String.t, offset :: integer, size :: integer}

    defstruct root: "", files: [], piece_size: 0, piece_hashes: [], blocks: %{}
  end

  def start_link do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  @spec add(binary, String.t, [file], binary, integer, integer) :: :ok
  def add(info_hash, root, files, piece_hashes, piece_size, block_size) do
    blocks = 
      files
      |> Enum.reduce(0, fn {_, size}, offset -> offset + size end) # calc total size
      |> chunk(piece_size) # chunk pieces into list of block sizes
      |> Enum.map(&chunk(&1, block_size))
      |> Enum.map(fn blocks ->
        Enum.map_reduce(blocks, 0, fn block_size, offset ->
          {{offset, block_size, :need}, block_size + offset}
        end) |> elem(0)
      end)
      |> Enum.reduce({%{}, 0}, fn block, {m, i} -> # reduce to map
        {Map.put(m, i, block), i+1}
      end)
      |> elem(0)
      
    files =
      files
      |> Enum.map_reduce(0, fn {fname, size}, offset ->
        {{fname, offset, size}, offset + size}
      end)
      |> elem(0)

    entry = %Entry{
      root: root,
      files: files,
      piece_size: piece_size,
      piece_hashes: piece_hashes,
      blocks: blocks
    }

    Agent.update(__MODULE__, &(Map.put_new(&1, info_hash, entry)))
  end

  def remove(info_hash) do
    Agent.update(__MODULE__, &Map.delete(&1, info_hash))
  end

  def reset do
    Agent.update(__MODULE__, fn _ -> %{} end)
  end

  @spec blocks(binary, integer) :: [block]
  def blocks(info_hash, piece_idx) do
    get_entry(info_hash, fn %{blocks: blocks} ->
      Map.get(blocks, piece_idx)
    end)
  end

  def segments(info_hash, piece_idx, offset, size) do
    get_entry(info_hash, fn %{root: root, files: files, blocks: blocks, piece_size: piece_size} ->
      offset = (piece_idx * piece_size) + offset
      files = Enum.filter(files, fn {_, off, fsize} -> offset <= off + fsize end)

      calc_segments(offset - (List.first(files) |> elem(1)), size, files, [])
      |> Enum.map(fn {fname, offset, size} ->
        {Path.join(root, fname), offset, size}
      end)
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

  @spec update_status(binary, integer, {integer, integer}, status) :: :ok
  def update_status(info_hash, piece_idx, {offset, size}, status) do
    update_piece(info_hash, piece_idx, fn blocks ->
      block_idx = Enum.find_index(blocks, fn {off, size, _} -> 
        offset == off && size == size
      end)

      List.update_at(blocks, block_idx, fn {offset, size, _} ->
        {offset, size, status}
      end)
    end)
  end

  @spec update_status(binary, integer, status) :: :ok
  def update_status(info_hash, piece_idx, status) do
    update_piece(info_hash, piece_idx, fn blocks ->
      Enum.map(blocks, fn {offset, size, _} ->
        {offset, size, status}
      end)
    end)
  end

  @spec update_root(binary, String.t) :: :ok
  def update_root(info_hash, new_root) do
    update_entry(info_hash, fn entry ->
      %{entry | root: new_root}
    end)
  end

  def piece_completed?(info_hash, piece_idx) do
    get_piece(info_hash, piece_idx, fn blocks ->
      Enum.filter(blocks, fn {_, _, status} -> status == :need end) == []
    end)
  end

  defp get_entry(info_hash, fun) do
    Agent.get(__MODULE__, &(Map.get(&1, info_hash) |> fun.()))
  end

  defp update_entry(info_hash, fun) do
    Agent.update(__MODULE__, &(Map.update!(&1, info_hash, fun)))
  end

  defp get_piece(info_hash, piece_idx, fun) do
    get_entry(info_hash, fn %{blocks: blocks} ->
      Map.get(blocks, piece_idx) |> fun.()
    end)
  end

  defp update_piece(info_hash, piece_idx, fun) do
    update_entry(info_hash, fn %{blocks: blocks} = entry ->
      %{entry | blocks: Map.update!(blocks, piece_idx, fun)}
    end)
  end

  defp chunk(total, chunk_size) when total < chunk_size do
    [total]
  end
  defp chunk(total, chunk_size) when rem(total, chunk_size) > 0 do
    r = rem(total, chunk_size)
    chunk(total - r, chunk_size) ++ [r]
  end
  defp chunk(total, chunk_size) do
    full_pieces = div(total, chunk_size)
    Enum.map(1..full_pieces, fn _ -> chunk_size end)
  end
end
