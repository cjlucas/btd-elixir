defmodule Torrent.Store do
  use GenServer

  def start_link do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  @spec add(Torrent.t) :: :ok
  def add(%{info_hash: info_hash} = torrent) do
    Agent.update(__MODULE__, &Map.put(&1, info_hash, torrent))
  end

  @spec remove(binary) :: :ok
  def remove(info_hash) do
    Agent.update(__MODULE__, &Map.delete(&1, info_hash))
  end

  @spec get(binary) :: Torrent.t | nil
  def get(info_hash) do
    Agent.get(__MODULE__, &Map.get(&1, info_hash))
  end
end
