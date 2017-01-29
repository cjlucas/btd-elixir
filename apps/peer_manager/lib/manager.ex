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

  def handle_info({:peer_connected, _conn}, state) do
    {:noreply, state}
  end

  def handle_info({:peer_disconnected, _conn}, state) do
    {:noreply, state}
  end

  def handle_info({:received_message, conn, %Bitfield{}}, state) do
    Peer.Connection.send_msg(conn, %Interested{})
    {:noreply, state}
  end

  def handle_info({:received_message, conn, %Unchoke{}}, %{pieces: [{index, begin, length} | rest]} = state) do
    Peer.Connection.send_msg(conn, %Request{index: index, begin: begin, length: length})
    {:noreply, %{state | pieces: rest}}
  end

  def handle_info({:received_message, conn, %Piece{index: index, begin: begin, block: block}}, %{info_hash: h, pieces: pieces} = state) do
    pieces =
      case pieces do
        [{index, begin, length} | rest] ->
          Peer.Connection.send_msg(conn, %Request{index: index, begin: begin, length: length})
          rest
        [] ->
          Logger.debug("Done sending pieces")
          []
      end

    FileManager.write_block(h, index, begin, block)
    Peer.Manager.Store.incr_downloaded(h, <<>>, byte_size(block))

    {:noreply, %{state | pieces: pieces}}
  end

  def handle_info({:received_message, conn, %Request{index: idx, begin: offset, length: len}}, %{info_hash: h} = state) do
    case FileManager.read_block(h, idx, offset, len) do
      {:ok, data} ->
        Peer.Connection.send_msg(conn, %Piece{index: offset, begin: offset, block: data})
      {:error, reason} ->
        Logger.debug("Read of requested block returned an error: #{reason}")
    end

    {:noreply, state}
  end

  def handle_info({:received_message, _conn, msg}, state) do
    {:noreply, state}
  end

  def handle_info({:sent_message, _conn, %Piece{block: block}}, %{info_hash: h} = state) do
    Peer.Manager.Store.incr_uploaded(h, byte_size(block))
    {:noreply, state}
  end

  def handle_info({:sent_message, _conn, msg}, state) do
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
end
