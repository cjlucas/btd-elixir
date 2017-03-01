defmodule Peer.Manager do
  use GenServer
  require Logger
  alias Bittorrent.Message.{Bitfield, Request, Interested, Piece, Have}

  @max_peers 50

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
    :ok = Peer.Manager.Store.add_peers(h, peers)
    connect_to_peers(h)

    {:reply, :ok, state}
  end

  def handle_info({:peer_connected, peer_id}, state) do
    IO.puts("GOT PEER #{inspect peer_id}")
    {:noreply, state}
  end

  def handle_info({:received_message, peer_id, %Piece{index: index, begin: begin, block: block}}, %{info_hash: h} = state) do
    #Logger.debug("GOT A PIECE #{inspect {index, begin}}")
    FileManager.write_block(h, index, begin, block)
    :ok = Peer.Manager.Store.incr_downloaded(h, byte_size(block))

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
    Peer.Swarm.Registry.lookup(info_hash)
    |> Enum.each(&Peer.Connection.send_msg(info_hash, &1, msg))
  end

  defp send_msg(info_hash, peer_id, msg) do
    Peer.Connection.send_msg(info_hash, peer_id, msg)
  end

  defp connect_to_peers(info_hash) do
    1..@max_peers-length(Peer.Swarm.Registry.lookup(info_hash))
    |> Enum.each(fn _ ->
      case Peer.Manager.Store.pop_peer(info_hash) do
        {host, port} ->
          Peer.Handshake.Supervisor.connect(host, port, info_hash)
        nil ->
          Logger.debug("No more available peers")
      end
    end)
  end

  defp via(info_hash) do
    {:via, Registry, {Peer.Manager.Registry, info_hash}}
  end
end
