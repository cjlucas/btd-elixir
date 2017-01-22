defmodule Peer.Manager do
  use GenServer
  require Logger
  alias Bittorrent.Message.{Bitfield, Unchoke, Request, Interested, Piece}

  @name __MODULE__

  defmodule State do
    defstruct info_hash: <<>>
  end

  def start_link(info_hash) do
    GenServer.start_link(@name, info_hash)
  end

  def init(info_hash) do
    Registry.register(Peer.Manager.Registry, info_hash, [])
    Peer.EventManager.register(info_hash)
    {:ok, %State{info_hash: info_hash}}
  end

  def handle_info({:received_connection, conn}, state) do
    {:noreply, state}
  end

  def handle_info({:received_message, conn, %Bitfield{}}, state) do
    Peer.Connection.send_msg(conn, %Interested{})
    {:noreply, state}
  end
  
  def handle_info({:received_message, conn, %Unchoke{}}, state) do
    Peer.Connection.send_msg(conn, %Request{index: 0, begin: 0, length: 16384})
    {:noreply, state}
  end
  
  def handle_info({:received_message, conn, %Piece{block: block}, %{info_hash: h} = state}) do
    Peer.Stats.Store.incr_downloaded(h, byte_size(block))
    {:noreply, state}
  end

  def handle_info({:received_message, _conn, msg}, state) do
    Logger.debug("Received unhandled message: #{inspect msg}")
    {:noreply, state}
  end

  def handle_info({:sent_message, conn, %Piece{block: block}, %{info_hash: h} = state}) do
    Peer.Stats.Store.incr_uploaded(h, byte_size(block))
    {:noreply, state}
  end
  
  def handle_info({:sent_message, _conn, msg}, state) do
    Logger.debug("Sent unhandled message: #{inspect msg}")
    {:noreply, state}
  end

  # TODO: why is this here? timeouts aren't even set
  def handle_info(:timeout, state) do
    {:stop, :normal, state}
  end

  def terminate(_reason, %{info_hash: h}) do
    Peer.Stats.Store.remove(h)
  end
end
