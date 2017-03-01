defmodule Peer.Swarm.PieceSetTest do
  alias Peer.Swarm.PieceSet
  use ExUnit.Case

  test "seen_piece/2, seen_pieces/2 and pieces_by_rarity/1" do
    {:ok, _} = PieceSet.start_link(<<>>)
    assert PieceSet.pieces_by_rarity(<<>>) == []
    assert PieceSet.seen_piece(<<>>, <<1>>, 0) == :ok
    assert PieceSet.pieces_by_rarity(<<>>) == [0]

    assert PieceSet.seen_piece(<<>>, <<2>>, 1) == :ok
    assert PieceSet.seen_piece(<<>>, <<3>>, 1) == :ok
    assert PieceSet.pieces_by_rarity(<<>>) == [1, 0]

    assert PieceSet.seen_pieces(<<>>, <<1>>, [2, 0]) == :ok
    assert PieceSet.seen_pieces(<<>>, <<2>>, [2, 0]) == :ok
    assert PieceSet.seen_pieces(<<>>, <<3>>, [2, 0]) == :ok
    assert PieceSet.pieces_by_rarity(<<>>) == [0, 2, 1]

    assert PieceSet.pieces(<<>>, <<1>>) == MapSet.new([0, 2])
    assert PieceSet.pieces(<<>>, <<2>>) == MapSet.new([0, 1, 2])
    assert PieceSet.pieces(<<>>, <<3>>) == MapSet.new([0, 1, 2])
  end
end
