defmodule TrackerEventHandler do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    TrackerManager.subscribe(:received_response)
    {:ok, []}
  end

  def handle_info({:received_response, info_hash, url, resp}, state) do
    %{peers: peers} = resp
    :ok = Swarm.Manager.add_peers(info_hash, peers)
    {:noreply, state}
  end
end
