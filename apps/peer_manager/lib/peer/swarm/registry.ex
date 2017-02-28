defmodule Peer.Swarm.Registry do

  @type info_hash :: binary

  @type peer_id :: binary

  def start_link do
    Registry.start_link(:duplicate, __MODULE__)
  end

  @spec register(info_hash, peer_id) :: :ok
  def register(info_hash, peer_id) do
    {:ok, _} = Registry.register(__MODULE__, info_hash, peer_id)
    :ok
  end

  @spec unregister(info_hash) :: :ok
  def unregister(info_hash) do
    Registry.unregister(__MODULE__, info_hash)
  end

  @spec lookup(info_hash) :: [peer_id]
  def lookup(info_hash) do
    Registry.lookup(__MODULE__, info_hash) |> Enum.map(&elem(&1, 1))
  end
end
