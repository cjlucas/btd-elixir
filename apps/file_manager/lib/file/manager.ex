defmodule File.Manager do
  alias File.Manager.Store
  require Logger

  @moduledoc """
  Manages file set for a given torrent
  """

  use GenServer

  def start_link(info_hash) do
    GenServer.start_link(__MODULE__, info_hash, name: via(info_hash))
  end

  def write_block(info_hash, piece_idx, offset, data) do
    via(info_hash) |> GenServer.call({:write_block, piece_idx, offset, data})
  end

  def read_block(info_hash, piece_idx, offset, length) do
    via(info_hash) |> GenServer.call({:read_block, piece_idx, offset, length})
  end

  def init(info_hash) do
    {:ok, info_hash}
  end

  def handle_call({:write_block, piece_idx, offset, data}, _from, info_hash) do
    results =
      Store.segments(info_hash, piece_idx, offset, byte_size(data))
      |> Enum.map_reduce(0, fn {fpath, offset, size}, block_offset ->
        {{fpath, offset, size, block_offset}, size + block_offset}
      end)
      |> elem(0)
      |> Enum.map(fn {fpath, offset, size, block_offset} ->
        <<_::bytes-size(block_offset), seg_data::bytes-size(size), _::binary>> = data
        Torrent.FileHandler.Manager.write(fpath, offset, seg_data)
      end)

    reply =
      case Enum.filter(results, &(&1 != :ok)) do
        [] ->
          :ok = Store.update_status(info_hash, piece_idx, {offset, byte_size(data)}, :have)
          if Store.piece_complete?(info_hash, piece_idx) do
            hash_check_piece(piece_idx, info_hash)
          else
            :ok
          end
        l  ->
          List.first(l)
      end

    {:reply, reply, info_hash}
  end

  def handle_call({:read_block, piece_idx, offset, length}, _from, info_hash) do
    {:reply, do_read_block(piece_idx, offset, length, info_hash), info_hash}
  end

  def terminate(_reason, info_hash) do
    info_hash
    |> Store.files
    |> Enum.each(&(Torrent.FileHandler.Manager.close(&1)))
  end

  defp hash_check_piece(piece_idx, info_hash) do
    length = info_hash
             |> Store.blocks(piece_idx)
             |> Enum.reduce(0, fn {_, size, _}, acc -> acc + size end)

    case do_read_block(piece_idx, 0, length, info_hash) do
      {:ok, data} ->
        expected_hash = Store.piece_hash(info_hash, piece_idx)
        {ret, status} =
          case :crypto.hash(:sha, data) do
            ^expected_hash ->
              File.EventManager.piece_completed(info_hash, piece_idx)
              {:ok, :verified}
            actual ->
              Logger.debug("Hash check failed actual: #{inspect actual}")
              {{:error, :hash_check_failed}, :need}
          end

        Store.update_status(info_hash, piece_idx, status)
        ret
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_read_block(piece_idx, offset, length, info_hash) do
    results =
      Store.segments(info_hash, piece_idx, offset, length)
      |> Enum.map(fn {fpath, offset, size} ->
        Torrent.FileHandler.Manager.read(fpath, offset, size)
      end)
      |> Enum.map(fn res ->
        case res do
          {:ok, data}      -> {:ok, data}
          {:error, reason} -> {:error, reason}
          :eof             -> {:error, :eof}
        end
      end)

    case Enum.filter(results, &(elem(&1, 0) != :ok)) do
      [] -> {:ok, Enum.reduce(results, <<>>, fn {:ok, data}, acc -> acc <> data end)}
      l  -> List.first(l)
    end
  end

  defp via(info_hash) do
    {:via, Registry, {File.Manager.Registry, info_hash}}
  end
end
