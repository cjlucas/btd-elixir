defmodule Peer.Swarm.Registry do

  def start_link do
    Registry.start_link(:duplicate, __MODULE__)
  end

  def register(info_hash, peer_id) do
    {:ok, _} = Registry.register(__MODULE__, info_hash, peer_id)
    :ok
  end

  def unregister(info_hash) do
    Registry.unregister(__MODULE__, info_hash)
  end

  def peers(info_hash) do
    Registry.lookup(info_hash) |> Enum.map(&elem(&1, 1))
  end
end
