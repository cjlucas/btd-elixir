defmodule Peer.BlockManager do

  @type info_hash :: binary

  @type block :: {piece_idx :: integer, offset :: integer, size :: integer}

  defmodule State do
    defstruct blocks: %{}, requested_blocks: %{}
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

      %State{blocks: blocks}
    end, name: via(info_hash))
  end

  def get_blocks(info_hash) do
    via(info_hash) |> Agent.get_and_update(fn state ->
      pieces = Peer.PieceRarity.pieces_by_rarity(info_hash)
    end)
  end

  defp via(info_hash) do
    key = {info_hash, :block_manager}
    {:via, Registry, {Peer.Manager.Registry, key}}
  end
end
