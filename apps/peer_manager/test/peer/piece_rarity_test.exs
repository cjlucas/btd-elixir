defmodule Peer.PieceRarityTest do
  alias Peer.PieceRarity
  use ExUnit.Case

  test "seen_piece/2, seen_pieces/2 and pieces_by_rarity/1" do
    {:ok, _} = PieceRarity.start_link(<<>>)
    assert PieceRarity.pieces_by_rarity(<<>>) == []
    assert PieceRarity.seen_piece(<<>>, 0) == :ok
    assert PieceRarity.pieces_by_rarity(<<>>) == [0]

    assert PieceRarity.seen_piece(<<>>, 1) == :ok
    assert PieceRarity.seen_piece(<<>>, 1) == :ok
    assert PieceRarity.pieces_by_rarity(<<>>) == [1, 0]

    assert PieceRarity.seen_pieces(<<>>, [2, 0]) == :ok
    assert PieceRarity.seen_pieces(<<>>, [2, 0]) == :ok
    assert PieceRarity.seen_pieces(<<>>, [2, 0]) == :ok
    assert PieceRarity.pieces_by_rarity(<<>>) == [0, 2, 1]
  end
end
