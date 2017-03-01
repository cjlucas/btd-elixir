defmodule Peer.Swarm.PieceSet do

  @type info_hash :: binary

  @type peer_id :: binary

  defmodule State do
    defstruct count_map: %{}, peer_pieces: %{}, rarity_list: nil
  end

  def start_link(info_hash) do
    Agent.start_link(fn -> %State{} end, name: via(info_hash))
  end

  @spec seen_piece(info_hash, peer_id, integer) :: :ok
  def seen_piece(info_hash, peer_id, piece_idx) do
    seen_pieces(info_hash, peer_id, [piece_idx])
  end

  @spec seen_piece(info_hash, peer_id, [integer]) :: :ok
  def seen_pieces(info_hash, peer_id, piece_idxs) do
    via(info_hash) |> Agent.update(fn %{count_map: m, peer_pieces: p} = state ->
      m = Enum.reduce(piece_idxs, m, fn idx, acc ->
        Map.update(acc, idx, 1, &(&1 + 1))
      end)

      pieces = Map.get(p, peer_id, MapSet.new)
      pieces = Enum.reduce(piece_idxs, pieces, fn idx, acc ->
        MapSet.put(acc, idx)
      end)

      %{state | count_map: m, peer_pieces: Map.put(p, peer_id, pieces), rarity_list: nil}
    end)
  end

  @spec pieces(info_hash, peer_id) :: MapSet.t
  def pieces(info_hash, peer_id) do
    via(info_hash) |> Agent.get(fn %{peer_pieces: p} ->
      Map.get(p, peer_id, MapSet.new)
    end)
  end

  @spec pieces_by_rarity(info_hash) :: [integer]
  def pieces_by_rarity(info_hash) do
    via(info_hash) |> Agent.get_and_update(fn
      %{count_map: m, rarity_list: l} = state when is_nil(l) ->
        l =
          m
          |> Enum.group_by(&elem(&1, 1))
          |> Enum.sort_by(&elem(&1, 0))
          |> Enum.map(&elem(&1, 1))
          |> Enum.reverse
          |> List.flatten
          |> Enum.map(&elem(&1, 0))

        {l, %{state | rarity_list: l}}
      %{rarity_list: l} = state ->
        {l, state}
    end)
  end

  defp via(info_hash) do
    key = {info_hash, :piece_set}
    {:via, Registry, {Peer.Manager.Registry, key}}
  end
end
