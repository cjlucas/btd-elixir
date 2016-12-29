defmodule Peer.Registry do
  use GenServer

  defmodule State do
    defstruct pid_map: %{}
  end

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def register(info_hash, peer_id, conn_pid) do
    id = :crypto.hash(:sha, [info_hash, peer_id])
    GenServer.call(__MODULE__, {:register, id, conn_pid})
  end

  def deregister(id) do
    GenServer.call(__MODULE__, {:deregister, id})
  end

  def lookup(id) do
    GenServer.call(__MODULE__, {:lookup, id})
  end

  def init(:ok) do
    {:ok, %State{}}
  end

  def handle_call({:register, id, pid}, _from, %{pid_map: map} = state) do
    {:reply, :ok, %{state | pid_map: Map.put(map, id, pid)}} 
  end

  def handle_call({:deregister, id}, _from, %{pid_map: map} = state) do
    {:reply, :ok, %{state | pid_map: Map.delete(map, id)}}
  end

  def handle_call({:lookup, id}, _from, %{pid_map: map} = state) do
    if Map.has_key?(map, id) do
      {:reply, {:ok, map[id]}, state}
    else
      {:reply, {:error, :not_found}, state}
    end
  end
end
