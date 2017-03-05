defmodule Swarm.Stats do
  defmodule State do
    defstruct skey_hash: <<>>,
    peer_id: <<>>,
    peers: MapSet.new,
    downloaded: 0,
    uploaded: 0
  end

  def start_link(info_hash) do
    Agent.start_link(fn ->
      skey_hash = Peer.HandshakeUtils.req2(info_hash)
      {:ok, _ } = Swarm.Stats.Registry.register_skey_hash(skey_hash, info_hash)

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

  @spec add_peers(binary, [{String.t, integer}]) :: :ok
  def add_peers(info_hash, new_peers) do
    via(info_hash) |> Agent.update(fn %{peers: peers} = state ->
      peers = new_peers |> Enum.reduce(peers, &MapSet.put(&2, &1))
      %{state | peers: peers}
    end)
  end

  @spec pop_peer(binary) :: {String.t, integer} | nil
  def pop_peer(info_hash) do
    via(info_hash) |> Agent.get_and_update(fn %{peers: peers} = state ->
      if MapSet.size(peers) > 0 do
        peer = Enum.random(peers)
        {peer, %{state | peers: MapSet.delete(peers, peer)}}
      else
        {nil, state}
      end
    end)
  end

  @spec incr_uploaded(binary, integer) :: :ok
  def incr_uploaded(info_hash, amnt) do
    via(info_hash) |> Agent.update(fn %{uploaded: up} = state ->
      %{state | uploaded: up + amnt}
    end)
  end

  @spec incr_downloaded(binary, integer) :: :ok
  def incr_downloaded(info_hash, amnt) do
    via(info_hash) |> Agent.update(fn %{downloaded: down} = state ->
      %{state | downloaded: down + amnt}
    end)
  end

  @spec stats(binary) :: [uploaded: integer, downloaded: integer]
  def stats(info_hash) do
    via(info_hash) |> Agent.get(fn %{uploaded: up, downloaded: down} ->
      [uploaded: up, downloaded: down]
    end)
  end

  defp via(info_hash) do
    {:via, Registry, {Swarm.Stats.Registry, {:info_hash, info_hash}}}
  end
end
