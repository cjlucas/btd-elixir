defmodule Peer.Manager.Store do
  defmodule PeerInfo do
    defstruct bitfield: MapSet.new, outstanding_reqs: MapSet.new
  end

  defmodule State do
    defstruct skey_hash: <<>>,
    peer_id: <<>>,
    piece_rarity_map: %{}, # piece # => instances seen
    piece_priority_list: [],
    peers: MapSet.new,
    downloaded: 0,
    uploaded: 0,
    missing_blocks: %{}, # piece_id => [block]
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

  def seen_bitfield(info_hash, peer_id, bitset) do
    via(info_hash) |> Agent.update(fn %{piece_rarity_map: m} = state ->
      m =
        0..Enum.count(bitset)-1
        |> Enum.reduce(m, fn idx, acc ->
          Map.update(acc, idx, 0, &(&1 + BitSet.get(bitset, idx)))
        end)

      %{state | piece_rarity_map: m, piece_priority_list: update_piece_priority_list(m)}
    end)

    update_connected_peer(info_hash, peer_id, fn peer ->
      bitfield =
        0..Enum.count(bitset)-1
        |> Enum.filter(&BitSet.get(bitset, &1) == 1)
        |> Enum.reduce(MapSet.new, fn idx, acc ->
          MapSet.put(acc, idx)
        end)

      %{peer | bitfield: bitfield}
    end)
  end

  @spec seen_piece(binary, binary, number) :: :ok
  def seen_piece(info_hash, peer_id, piece_idx) do
    via(info_hash) |> Agent.update(fn %{piece_rarity_map: m} = state ->
      m = Map.update(m, piece_idx, 0, &(&1 + 1))
      %{state | piece_rarity_map: m, piece_priority_list: update_piece_priority_list(m)}
    end)

    update_connected_peer(info_hash, peer_id, fn %{bitfield: bf} = peer ->
      %{peer | bitfield: MapSet.put(bf, piece_idx)}
    end)
  end

  defp update_piece_priority_list(map) do
    map
    |> Enum.group_by(&elem(&1, 1))
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(&elem(&1, 1))
    |> Enum.reverse
    |> List.flatten
    |> Enum.map(&elem(&1, 0))
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

  @type block :: {piece_idx :: number, offset :: number, size :: number}

  @spec set_missing_blocks(binary, [block]) :: :ok
  def set_missing_blocks(info_hash, blocks) do
    via(info_hash) |> Agent.update(fn state ->
      Map.put(state, :missing_blocks, Enum.group_by(blocks, &elem(&1, 0)))
    end)
  end

  def put_missing_block(info_hash, {piece_idx, _, _} = block) do
    via(info_hash) |> Agent.update(fn %{missing_blocks: blocks} = state ->
      %{state | missing_blocks: Map.update!(blocks, piece_idx, &[block | &1])}
    end)
  end

  def pop_missing_block(info_hash, peer_id, piece_idx) do
    via(info_hash) |> Agent.get_and_update(fn %{missing_blocks: blocks} = state ->
      case Map.get(blocks, piece_idx) do
        [head | tail] ->
          {head, %{state | missing_blocks: Map.put(blocks, piece_idx, tail)}}
        [] ->
          do_pop_missing_block(peer_id, state)
      end
    end)
  end

  def pop_missing_block(info_hash, peer_id) do
    via(info_hash) |> Agent.get_and_update(fn state ->
      do_pop_missing_block(peer_id, state)
    end)
  end

  @limit 100

  defp do_pop_missing_block(peer_id, %{piece_priority_list: m, missing_blocks: blocks, connected_peers: peers} = state) do
    %{bitfield: peer_pieces} = Map.get(peers, peer_id)

    candidates =
      m
      |> Enum.reduce_while([], fn idx, acc ->
        acc = if MapSet.member?(peer_pieces, idx) && !(Map.get(blocks, idx, []) |> Enum.empty?) do
          [idx | acc]
        else
          acc
        end

        if length(acc) == @limit do
          {:halt, acc}
        else
          {:cont, acc}
        end
      end)

    piece_idx = case candidates do
      [] -> nil
      c  -> Enum.random(c)
    end

    case Map.get(blocks, piece_idx) do
      [head | tail] ->
        {head, %{state | missing_blocks: Map.put(blocks, piece_idx, tail)}}
      [] ->
        {nil, state}
      nil ->
        {nil, state}
    end
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
