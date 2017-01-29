defmodule Peer.Registry do
  def start_link do
    Registry.start_link(:duplicate, __MODULE__)
  end

  @spec register(binary, binary) :: :ok
  def register(info_hash, peer_id) when is_binary(info_hash) and is_binary(peer_id) do
    {:ok, _} = Registry.register(__MODULE__, info_hash, peer_id)
    :ok
  end

  @spec lookup(binary) :: [pid]
  def lookup(info_hash) do
    Registry.lookup(__MODULE__, info_hash)
    |> Enum.map(fn entry -> elem(entry, 0) end)
  end

  @spec lookup(binary, binary) :: pid | nil
  def lookup(info_hash, peer_id) do
    Registry.match(__MODULE__, info_hash, peer_id)
    |> Enum.map(fn entry -> elem(entry, 0) end)
    |> List.first
  end
end
