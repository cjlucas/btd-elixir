defmodule Peer.BlockManager do

  @type info_hash :: binary

  @type block :: {piece_idx :: integer, offset :: integer, size :: integer}

  defmodule State do
    defstruct blocks: %{}, requested_blocks: %{}

    def has_available_blocks?(state, piece_idx) do
      %{blocks: blocks, requested_blocks: requested_blocks} = state

      blocks           = Map.get(blocks, piece_idx, MapSet.new)
      requested_blocks = Map.get(requested_blocks, piece_idx, MapSet.new)

      MapSet.size(blocks) > MapSet.size(requested_blocks)
    end

    def available_blocks(state, piece_idx) do
      %{blocks: blocks, requested_blocks: requested_blocks} = state

      blocks           = Map.get(blocks, piece_idx, MapSet.new)
      requested_blocks = Map.get(requested_blocks, piece_idx, MapSet.new)

      MapSet.difference(blocks, requested_blocks)
    end
  end

  def start_link(info_hash) do
    Agent.start_link(fn ->
      blocks =
        FileManager.pieces(info_hash)
        |> Enum.with_index
        |> Enum.flat_map(fn {blocks, piece_idx} ->
          Enum.map(blocks, &Tuple.insert_at(&1, 0, piece_idx))
        end)
        |> Enum.filter(fn {_, _, _, status} -> status == :need end)
        |> Enum.map(&Tuple.delete_at(&1, 3))
        |> Enum.group_by(&elem(&1, 0))
        |> Enum.map(fn {k, v} -> {k, MapSet.new(v)} end)
        |> Enum.into(%{})

      %State{blocks: blocks}
    end, name: via(info_hash))
  end

  def get_blocks(info_hash, peer_id, num_blocks, piece_idx \\ -1) do
    via(info_hash) |> Agent.get_and_update(fn state ->
      pieces           = Peer.Swarm.PieceSet.pieces_by_rarity(info_hash)
      available_pieces = Peer.Swarm.PieceSet.pieces(info_hash, peer_id)

      {blocks, _, state} =
        1..num_blocks
        |> Enum.reduce_while({[], piece_idx, state}, fn _, {chosen, idx, state} = acc ->
          %{blocks: blocks, requested_blocks: requested_blocks} = state

          piece_candidates =
            if State.has_available_blocks?(state, idx) do
              [idx]
            else
              pieces
              |> Enum.reduce_while([], fn idx, acc ->
                blocks     = Map.get(blocks, idx, MapSet.new)
                req_blocks = Map.get(requested_blocks, idx, MapSet.new)

                acc =
                  if MapSet.member?(available_pieces, idx) && State.has_available_blocks?(state, idx) do
                    [idx | acc]
                  else
                    acc
                  end

                  # Halt once we've hit our max # of candidate pieces
                  if length(acc) == 100 do
                    {:halt, acc}
                  else
                    {:cont, acc}
                  end
              end)
            end

            case piece_candidates do
              [] ->
                {:halt, acc}
              c ->
                idx    = Enum.random(c)
                blocks = State.available_blocks(state, idx)
                {idx, _, _} = block = Enum.random(blocks)

                requested_blocks = Map.update(requested_blocks, idx, MapSet.new([block]), &MapSet.put(&1, block))
                acc = {[block | chosen], idx, %{state | requested_blocks: requested_blocks}}
                {:cont, acc}
            end
        end)

        {blocks, state}
    end)
  end

  defp via(info_hash) do
    key = {info_hash, :block_manager}
    {:via, Registry, {Peer.Manager.Registry, key}}
  end
end
