defmodule Peer.Manager.Store do
  defmodule PeerInfo do
    defstruct bitfield: %BitSet{}, outstanding_reqs: MapSet.new
  end

  defmodule State do
    defstruct skey_hash: <<>>,
    peer_id: <<>>,
    piece_rarity_map: %{}, # piece # => instances seen
    piece_rarity_tiers: [], # list of list of piece #s
    peers: MapSet.new,
    downloaded: 0,
    uploaded: 0,
    connected_peers: %{} # peer_id => PeerInfo
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

  def peer_bitfield(info_hash, peer_id) do
    get_connected_peer(info_hash, peer_id, fn %{bitfield: bf} -> bf end)
  end

  @spec seen_piece(binary, binary, number) :: :ok
  def seen_piece(info_hash, peer_id, piece_idx) do
    via(info_hash) |> Agent.update(fn %{piece_rarity_map: m} = state ->
      %{state | piece_rarity_map: Map.update(m, piece_idx, 1, &(&1 + 1)), piece_rarity_tiers: []}
    end)

    #update_connected_peer(info_hash, peer_id, fn %{bitfield: bf} = peer ->
      #%{peer | bitfield: BitSet.set(bf, piece_idx, 1)}
    #end)
  end

  @spec pieces_by_rarity(binary) :: [[number]]
  def pieces_by_rarity(info_hash) do
    via(info_hash) |> Agent.get_and_update(fn %{piece_rarity_map: m, piece_rarity_tiers: t} = state ->
      case t do
        [] ->
          tiers =
            m
            |> Enum.group_by(&elem(&1, 1))
            |> Enum.sort_by(&elem(&1, 0))
            |> Enum.map(&elem(&1, 1))
            |> Enum.map(fn x -> Enum.map(x, &elem(&1, 0)) end)
            |> Enum.reverse

          {tiers, %{state | piece_rarity_tiers: tiers}}
        _ ->
          {t, state}
      end
    end)
  end

  @spec stats(binary) :: [uploaded: integer, downloaded: integer]
  def stats(info_hash) do
    via(info_hash) |> Agent.get(fn %{uploaded: up, downloaded: down} ->
      [uploaded: up, downloaded: down]
    end)
  end

  @spec requested_block(binary, binary, {number, number, number}) :: :ok
  def requested_block(info_hash, peer_id, block) do
    update_connected_peer(info_hash, peer_id, fn %{outstanding_reqs: reqs} = info ->
      %{info | outstanding_reqs: MapSet.put(reqs, block)}
    end)
  end

  @spec received_block(binary, binary, {number, number, number}) :: :ok
  def received_block(info_hash, peer_id, block) do
    update_connected_peer(info_hash, peer_id, fn %{outstanding_reqs: reqs} = info ->
      %{info | outstanding_reqs: MapSet.delete(reqs, block)}
    end)
  end

  @spec outstanding_requests(binary) :: [{number, number, number}]
  def outstanding_requests(info_hash) do
    via(info_hash) |> Agent.get(fn %{connected_peers: peers} ->
      Map.values(peers)
      |> Enum.reduce(MapSet.new, fn %{outstanding_reqs: reqs}, agg ->
        MapSet.union(agg, reqs)
      end)
    end)
  end

  @spec outstanding_requests(binary, binary) :: [{number, number, number}]
  def outstanding_requests(info_hash, peer_id) do
    get_connected_peer(info_hash, peer_id, fn %{outstanding_reqs: reqs} ->
      reqs
    end)
  end

  @spec remove_peer(binary, binary) :: :ok
  def remove_peer(info_hash, peer_id) do
    via(info_hash) |> Agent.update(fn %{connected_peers: peers} = state ->
      %{state | connected_peers: Map.delete(peers, peer_id)}
    end)
  end

  defp get_connected_peer(info_hash, peer_id, fun) do
    via(info_hash) |> Agent.get(fn %{connected_peers: peers} ->
      Map.get(peers, peer_id, %PeerInfo{}) |> fun.()
    end)
  end

  defp update_connected_peer(info_hash, peer_id, fun) do
    via(info_hash) |> Agent.update(fn %{connected_peers: peers} = state ->
      peer_info = Map.get(peers, peer_id, %PeerInfo{}) |> fun.()
      %{state | connected_peers: Map.put(peers, peer_id, peer_info)}
    end)
  end

  defp via(info_hash) do
    {:via, Registry, {Peer.Manager.Store.Registry, {:info_hash, info_hash}}}
  end
end
