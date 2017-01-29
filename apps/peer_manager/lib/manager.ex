defmodule Peer.Manager do
  use GenServer
  require Logger
  alias Bittorrent.Message.{Bitfield, Unchoke, Request, Interested, Piece, Have}

  @name __MODULE__

  defmodule State do
    defstruct info_hash: <<>>, pieces: []
  end

  def start_link(info_hash) do
    GenServer.start_link(@name, info_hash)
  end

  def init(info_hash) do
    Registry.register(Peer.Manager.Registry, info_hash, [])
    {:ok, _} = Peer.EventManager.register(info_hash)
    {:ok, _} = File.EventManager.register(info_hash)

    pieces =
      FileManager.pieces(info_hash)
      |> Enum.flat_map_reduce(0, fn blocks, i ->
        blocks = Enum.map(blocks, fn {offset, size, _} -> {i, offset, size} end)
        {blocks, i + 1}
      end)
      |> elem(0)

    {:ok, %State{info_hash: info_hash, pieces: pieces}}
  end

  def handle_info({:peer_connected, peer_id}, %{info_hash: info_hash} = state) do
    :ok = Peer.Manager.Store.add_peer(info_hash, peer_id)
    {:noreply, state}
  end

  def handle_info({:peer_disconnected, _peer_id}, state) do
    {:noreply, state}
  end

  def handle_info({:received_message, peer_id, %Bitfield{}}, %{info_hash: info_hash} = state) do
    send_msg(info_hash, peer_id, %Interested{})
    {:noreply, state}
  end

  def handle_info({:received_message, peer_id, %Unchoke{}}, %{info_hash: info_hash, pieces: [{index, begin, length} | rest]} = state) do
    send_msg(info_hash, peer_id, %Request{index: index, begin: begin, length: length})
    {:noreply, %{state | pieces: rest}}
  end

  def handle_info({:received_message, peer_id, %Piece{index: index, begin: begin, block: block}}, %{info_hash: h, pieces: pieces} = state) do
    pieces =
      case pieces do
        [{index, begin, length} | rest] ->
          send_msg(h, peer_id, %Request{index: index, begin: begin, length: length})
          rest
        [] ->
          Logger.debug("Done sending pieces")
          []
      end

    FileManager.write_block(h, index, begin, block)
    :ok = Peer.Manager.Store.incr_downloaded(h, peer_id, byte_size(block))

    {:noreply, %{state | pieces: pieces}}
  end

  def handle_info({:received_message, peer_id, %Request{index: idx, begin: offset, length: len}}, %{info_hash: h} = state) do
    case FileManager.read_block(h, idx, offset, len) do
      {:ok, data} ->
        send_msg(h, peer_id, %Piece{index: offset, begin: offset, block: data})
      {:error, reason} ->
        Logger.debug("Read of requested block returned an error: #{reason}")
    end

    {:noreply, state}
  end

  def handle_info({:received_message, _peer_id, _msg}, state) do
    {:noreply, state}
  end

  def handle_info({:sent_message, peer_id, %Piece{block: block}}, %{info_hash: h} = state) do
    Peer.Manager.Store.incr_uploaded(h, peer_id, byte_size(block))
    {:noreply, state}
  end

  def handle_info({:sent_message, _peer_id, _msg}, state) do
    #Logger.debug("Sent message #{inspect msg}")
    {:noreply, state}
  end

  # TODO: why is this here? timeouts aren't even set
  def handle_info(:timeout, state) do
    {:stop, :normal, state}
  end

  def handle_info({:piece_completed, info_hash, piece_idx}, state) do
    dispatch_msg(info_hash, %Have{index: piece_idx})
    {:noreply, state}
  end

  defp dispatch_msg(info_hash, msg) do
    Peer.Registry.lookup(info_hash) |> Enum.each(&Peer.Connection.send_msg(&1, msg))
  end

  defp send_msg(info_hash, peer_id, msg) do
    Peer.Registry.lookup(info_hash, peer_id) |> Peer.Connection.send_msg(msg)
  end
end
