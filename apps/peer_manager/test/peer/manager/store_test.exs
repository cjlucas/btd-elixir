defmodule Peer.Manager.NewStoreTest do
  alias Peer.Manager.Store
  use ExUnit.Case

  setup do
    {:ok, _} = Store.start_link(<<>>)
    :ok
  end

  test "add_peers/2 and pop_peer/1 with single peer" do
    # test single peer
    assert Store.pop_peer(<<>>) |> is_nil
    assert Store.add_peers(<<>>, [{"127.0.0.1", 50000}]) == :ok
    assert Store.pop_peer(<<>>) == {"127.0.0.1", 50000}
    assert Store.pop_peer(<<>>) |> is_nil
  end

  test "add_peers/2 and pop_peer/1 with multiple peers" do
    assert Store.add_peers(<<>>, [{"127.0.0.1", 50000}, {"8.8.8.8", 50000}]) == :ok
    refute Store.pop_peer(<<>>) |> is_nil
    refute Store.pop_peer(<<>>) |> is_nil
    assert Store.pop_peer(<<>>) |> is_nil
  end

  test "add_peers/2 and pop_peer/1 with duplicate peers" do
    assert Store.add_peers(<<>>, [{"127.0.0.1", 50000}, {"127.0.0.1", 50000}]) == :ok
    refute Store.pop_peer(<<>>) |> is_nil
    assert Store.pop_peer(<<>>) |> is_nil
  end

  test "peer_id/1" do
    assert Store.peer_id(<<>>) |> is_binary
  end

  test "incr_uploaded/3 and incr_downloaded/3 and stats/1" do
    assert Store.incr_uploaded(<<>>, 5) == :ok
    assert Store.stats(<<>>) == [uploaded: 5, downloaded: 0]

    assert Store.incr_uploaded(<<>>, 10) == :ok
    assert Store.stats(<<>>) == [uploaded: 15, downloaded: 0]

    assert Store.incr_downloaded(<<>>, 5) == :ok
    assert Store.stats(<<>>) == [uploaded: 15, downloaded: 5]

    assert Store.incr_downloaded(<<>>, 10) == :ok
    assert Store.stats(<<>>) == [uploaded: 15, downloaded: 15]
  end

  test "seen_piece/2 and pieces_by_rarity/1" do
    assert Store.pieces_by_rarity(<<>>) == []
    assert Store.seen_piece(<<>>, 1) == :ok
    assert Store.pieces_by_rarity(<<>>) == [[1]]
    assert Store.seen_piece(<<>>, 2) == :ok
    assert Store.pieces_by_rarity(<<>>) == [[1, 2]]
    assert Store.seen_piece(<<>>, 1) == :ok
    assert Store.pieces_by_rarity(<<>>) == [[1], [2]]
  end
end
