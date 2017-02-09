defmodule Peer.Manager.Store do
  defmodule State do
    defstruct skey_hash: <<>>,
    peer_id: <<>>,
    piece_rarity_map: %{}, # piece # => instances seen
    piece_rarity_tiers: [], # list of list of piece #s
    peers: MapSet.new,
    downloaded: 0,
    uploaded: 0,
    outstanding_reqs: %{} # peer id => MapSet of {idx, offset, size}
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

  @spec seen_piece(binary, number) :: :ok
  def seen_piece(info_hash, piece_idx) do
    via(info_hash) |> Agent.update(fn %{piece_rarity_map: m} = state ->
      %{state | piece_rarity_map: Map.update(m, piece_idx, 1, &(&1 + 1)), piece_rarity_tiers: []}
    end)
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
    via(info_hash) |> Agent.update(fn %{outstanding_reqs: reqs} = state ->
      reqs = Map.update(reqs, peer_id, MapSet.new([block]), &MapSet.put(&1, block))
      %{state | outstanding_reqs: reqs}
    end)
  end

  @spec received_block(binary, binary, {number, number, number}) :: :ok
  def received_block(info_hash, peer_id, block) do
    via(info_hash) |> Agent.update(fn %{outstanding_reqs: reqs} = state ->
      reqs = Map.update(reqs, peer_id, MapSet.new, &MapSet.delete(&1, block))
      %{state | outstanding_reqs: reqs}
    end)
  end

  @spec outstanding_requests(binary) :: [{number, number, number}]
  def outstanding_requests(info_hash) do
    via(info_hash) |> Agent.get(fn %{outstanding_reqs: reqs} ->
      Enum.reduce(reqs, MapSet.new, fn {_peer_id, reqs}, agg ->
        MapSet.union(agg, reqs)
      end)
    end)
  end

  @spec outstanding_requests(binary, binary) :: [{number, number, number}]
  def outstanding_requests(info_hash, peer_id) do
    via(info_hash) |> Agent.get(fn %{outstanding_reqs: reqs} ->
      Map.get(reqs, peer_id, MapSet.new)
    end)
  end

  defp via(info_hash) do
    {:via, Registry, {Peer.Manager.Store.Registry, {:info_hash, info_hash}}}
  end
end
