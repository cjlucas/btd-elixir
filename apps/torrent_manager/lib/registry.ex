defmodule Torrent.Registry do
  use GenServer

  defmodule State do
    defstruct info_hash_map: %{}, skey_hash_map: %{}
  end

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def size do
    GenServer.call(__MODULE__, :size)
  end

  def reset do
    GenServer.call(__MODULE__, :reset)
  end

  def register(torrent) do
    GenServer.call(__MODULE__, {:register, torrent})
  end

  def deregister(info_hash) do
    GenServer.call(__MODULE__, {:deregister, info_hash})
  end

  def lookup({:info_hash, hash}) do
    GenServer.call(__MODULE__, {:lookup, :info_hash, hash})
  end

  def lookup({:skey_hash, hash}) do
    GenServer.call(__MODULE__, {:lookup, :skey_hash, hash})
  end

  def init(:ok) do
    {:ok, %State{}}
  end

  def handle_call(:size, _from, %{info_hash_map: map} = state) do
    {:reply, Map.size(map), state}
  end

  def handle_call(:reset, _from, _state) do
    [Torrent.Store.Supervisor, Torrent.Manager.Supervisor]
    |> Enum.each(fn sup ->
      for {_, pid, _, _} <- Supervisor.which_children(sup) do
        Supervisor.terminate_child(sup, pid)
      end
    end)
    {:reply, :ok, %State{}}
  end

  def handle_call({:register, %{info_hash: info_hash} = torrent}, _from, state) do
    if Map.has_key?(state.info_hash_map, info_hash) do
      {:reply, {:error, :already_exists}, state}
    else
      {:ok, pid1} = Torrent.Store.Supervisor.start_child(torrent)
      {:ok, pid2} = Torrent.Manager.Supervisor.start_child(info_hash)

      val = {pid1, pid2}
      state = %{state | info_hash_map: Map.put(state.info_hash_map, info_hash, val)}
      state = %{state | skey_hash_map: Map.put(state.skey_hash_map, skey_hash(info_hash), val)}

      {:reply, {:ok, {pid1, pid2}}, state}
    end
  end

  def handle_call({:deregister, info_hash}, _from, state) do
    if Map.has_key?(state.info_hash_map, info_hash) do
      {pid1, pid2} = Map.get(state.info_hash_map, info_hash)
      :ok = Supervisor.terminate_child(Torrent.Store.Supervisor, pid1)
      :ok = Supervisor.terminate_child(Torrent.Manager.Supervisor, pid2)

      info_hash_map = Map.delete(state.info_hash_map, info_hash)
      skey_hash_map = Map.delete(state.skey_hash_map, skey_hash(info_hash))
      {:reply, :ok, %{state | info_hash_map: info_hash_map, skey_hash_map: skey_hash_map}}
    else
      {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:lookup, :info_hash, hash}, _from, %{info_hash_map: map} = state) do
    if Map.has_key?(map, hash) do
      {:reply, {:ok, map[hash]}, state}
    else
      {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:lookup, :skey_hash, hash}, _from, %{skey_hash_map: map} = state) do
    if Map.has_key?(map, hash) do
      {:reply, {:ok, map[hash]}, state}
    else
      {:reply, {:error, :not_found}, state}
    end
  end

  defp skey_hash(info_hash) do
    :crypto.hash(:sha, [<<"req2">>, info_hash])
  end
end
