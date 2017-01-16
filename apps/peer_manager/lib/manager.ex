defmodule Peer.Manager do
  use GenServer

  @name __MODULE__

  defmodule State do
    defstruct []
  end

  def start_link(info_hash) do
    GenServer.start_link(@name, info_hash)
  end

  def init(info_hash) do
    Registry.register(Peer.Manager.Registry, info_hash, [])
    #Peer.EventManager.register(info_hash)
    {:ok, %State{}}
  end

  def handle_info({:received_connection, conn}, state) do
    IO.puts("Received new connection")
    {:noreply, state}
  end

  def handle_info(:timeout, state) do
    {:stop, :normal, state}
  end
end
