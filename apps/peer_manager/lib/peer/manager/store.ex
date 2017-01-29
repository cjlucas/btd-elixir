defmodule Peer.Manager.Store do
  defmodule PeerInfo do
    defstruct uploaded: 0, downloaded: 0
  end

  defmodule State do
    defstruct skey_hash: <<>>,
    peer_id: <<>>,
    peers: MapSet.new,
    known_peers: %{}
  end

  def start_link(info_hash) do
    Agent.start_link(fn ->
      skey_hash = Peer.HandshakeUtils.req2(info_hash)
      {:ok, _ } = Peer.Manager.Store.Registry.register_skey_hash(skey_hash, info_hash)

      %State{
        skey_hash: skey_hash,
        peer_id: :crypto.strong_rand_bytes(20),
      }
    end, name: via(info_hash))
  end

  @spec peer_id(binary) :: binary
  def peer_id(info_hash) do
    via(info_hash) |> Agent.get(fn %{peer_id: peer_id} -> peer_id end)
  end

  @spec add_peer(binary, binary) :: :ok
  def add_peer(info_hash, peer_id) do
    via(info_hash) |> Agent.update(fn %{known_peers: peers} = state ->
      %{state | known_peers: Map.put_new(peers, peer_id, %PeerInfo{})}
    end)
  end

  @spec has_peer?(binary, binary) :: boolean
  def has_peer?(info_hash, peer_id) do
    via(info_hash) |> Agent.get(fn %{known_peers: peers} ->
      Map.has_key?(peers, peer_id)
    end)
  end

  @spec incr_uploaded(binary, binary, integer) :: :ok
  def incr_uploaded(info_hash, peer_id, amnt) do
    update_peer(info_hash, peer_id, fn peer ->
      Map.update!(peer, :uploaded, &(&1 + amnt))
    end)
  end

  @spec incr_downloaded(binary, binary, integer) :: :ok
  def incr_downloaded(info_hash, peer_id, amnt) do
    update_peer(info_hash, peer_id, fn peer ->
      Map.update!(peer, :downloaded, &(&1 + amnt))
    end)
  end

  @spec stats(binary) :: [uploaded: integer, downloaded: integer]
  def stats(info_hash) do
    via(info_hash) |> Agent.get(fn %{known_peers: peers} ->
      peers
      |> Map.values
      |> Enum.map(fn %{uploaded: up, downloaded: dn} -> {up, dn} end)
      |> Enum.unzip
      |> Tuple.to_list
      |> Enum.map(&Enum.sum/1)
      |> Enum.zip([:uploaded, :downloaded])
      |> Enum.map(fn {sum, key} -> {key, sum} end)
    end)
  end

  defp update_peer(info_hash, peer_id, fun) do
    via(info_hash) |> Agent.update(fn %{known_peers: peers} = state ->
      %{state | known_peers: Map.update!(peers, peer_id, fun)}
    end)
  end

  defp via(info_hash) do
    {:via, Registry, {Peer.Manager.Store.Registry, {:info_hash, info_hash}}}
  end
end
