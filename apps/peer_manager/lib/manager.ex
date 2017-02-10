defmodule Peer.Manager do
  use GenServer
  require Logger
  alias Bittorrent.Message.{Bitfield, Unchoke, Request, Interested, Piece, Have}

  defmodule State do
    defstruct info_hash: <<>>
  end

  def start_link(info_hash) do
    GenServer.start_link(__MODULE__, info_hash, name: via(info_hash))
  end

  def add_peers(info_hash, peers) do
    via(info_hash) |> GenServer.call({:add_peers, peers})
  end

  def init(info_hash) do
    {:ok, _} = Peer.EventManager.register(info_hash)
    {:ok, _} = File.EventManager.register(info_hash)

    {:ok, %State{info_hash: info_hash}}
  end

  def handle_call({:add_peers, peers}, _from, %{info_hash: h} = state) do
    {:reply, Peer.Manager.Store.add_peers(h, peers), state}
  end

  def handle_info({:peer_connected, _peer_id}, state) do
    {:noreply, state}
  end

  def handle_info({:peer_disconnected, peer_id}, %{info_hash: h} = state) do
    :ok = Peer.Manager.Store.remove_peer(h, peer_id)
    {:noreply, state}
  end

  def handle_info({:received_message, peer_id, %Bitfield{bitfield: bits}}, %{info_hash: info_hash} = state) do
    bs = BitSet.from_binary(bits)

    0..length(FileManager.pieces(info_hash))-1
    |> Enum.map(&{&1, BitSet.get(bs, &1)})
    |> Enum.filter(&elem(&1, 1))
    |> Enum.map(&elem(&1, 0))
    |> Enum.each(&Peer.Manager.Store.seen_piece(info_hash, &1))

    send_msg(info_hash, peer_id, %Interested{})
    {:noreply, state}
  end

  def handle_info({:received_message, _peer_id, %Have{index: idx}}, %{info_hash: info_hash} = state) do
    :ok = Peer.Manager.Store.seen_piece(info_hash, idx)
    {:noreply, state}
  end

  def handle_info({:received_message, _peer_id, %Unchoke{}}, state) do
    {:noreply, state}
  end

  def handle_info({:received_message, peer_id, %Piece{index: index, begin: begin, block: block}}, %{info_hash: h} = state) do
    FileManager.write_block(h, index, begin, block)
    :ok = Peer.Manager.Store.incr_downloaded(h, byte_size(block))
    :ok = Peer.Manager.Store.received_block(h, peer_id, {index, begin, block})

    {:noreply, state}
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

  def handle_info({:sent_message, _peer_id, %Piece{block: block}}, %{info_hash: h} = state) do
    Peer.Manager.Store.incr_uploaded(h, byte_size(block))
    {:noreply, state}
  end

  def handle_info({:sent_message, peer_id, %Request{index: idx, begin: offset, length: len}}, %{info_hash: h} = state) do
    :ok = Peer.Manager.Store.requested_block(h, peer_id, {idx, offset, len})
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

  defp via(info_hash) do
    {:via, Registry, {Peer.Manager.Registry, info_hash}}
  end
end
