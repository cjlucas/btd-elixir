defmodule Torrent.Registry do
  use GenServer

  defmodule State do
    defstruct info_hash_map: %{}, skey_hash_map: %{}
  end

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def register(info_hash, pid) do
    GenServer.call(__MODULE__, {:register, info_hash, pid})
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

  def handle_call({:register, info_hash, pid}, _from, state) do
    if Map.has_key?(state.info_hash_map, info_hash) do
      {:reply, {:error, :already_has_key}, state}
    else
      state = put_in(state, [:info_hash_map, info_hash], pid)
      state = put_in(state, [:skey_hash_map, skey_hash(info_hash)], pid)
      {:reply, :ok, state}
    end
  end
  
  def handle_call({:deregister, info_hash}, _from, state) do
    if Map.has_key?(state.info_hash_map, info_hash) do
      info_hash_map = Map.delete(state.info_hash_map, info_hash)
      skey_hash_map = Map.delete(state.skey_hash_map, skey_hash(info_hash))
      {:reply, :ok, %{state | info_hash_map: info_hash_map, skey_hash_map: skey_hash_map}}
    else
      {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:lookup, :info_hash, hash}, _from, %{info_hash_map: map} = state) do
    if Map.has_key?(map, hash) do
      {:reply, {:ok, map[hash], state}}
    else
      {:reply, {:error, :not_found}, state}
    end
  end
  
  def handle_call({:lookup, :skey_hash, hash}, _from, %{skey_hash_map: map} = state) do
    if Map.has_key?(map, hash) do
      {:reply, {:ok, map[hash], state}}
    else
      {:reply, {:erorr, :not_found}, state}
    end
  end

  defp skey_hash(info_hash) do
    :crypto.hash(:sha, [<<"req2">>, info_hash])
  end
end
