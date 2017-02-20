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
    assert Store.seen_piece(<<>>, <<1>>, 1) == :ok
    assert Store.pieces_by_rarity(<<>>) == [[1]]
    assert Store.seen_piece(<<>>, <<1>>, 2) == :ok
    assert Store.pieces_by_rarity(<<>>) == [[1, 2]]
    assert Store.seen_piece(<<>>, <<1>>, 1) == :ok
    assert Store.pieces_by_rarity(<<>>) == [[1], [2]]
  end

  test "requested_block/3, received_block/3, outstanding_requests/1 and outstanding_requests/2" do
    assert Store.outstanding_requests(<<>>) |> MapSet.size == 0
    assert Store.outstanding_requests(<<>>, <<1>>) |> MapSet.size == 0

    assert Store.requested_block(<<>>, <<1>>, {0, 0, 3}) == :ok
    assert Store.outstanding_requests(<<>>) == MapSet.new([{0, 0, 3}])
    assert Store.outstanding_requests(<<>>, <<1>>) == MapSet.new([{0, 0, 3}])

    assert Store.requested_block(<<>>, <<1>>, {1, 0, 3}) == :ok
    assert Store.outstanding_requests(<<>>) == MapSet.new([{0, 0, 3}, {1, 0, 3}])
    assert Store.outstanding_requests(<<>>, <<1>>) == MapSet.new([{0, 0, 3}, {1, 0, 3}])

    assert Store.requested_block(<<>>, <<2>>, {0, 0, 3}) == :ok
    assert Store.outstanding_requests(<<>>) == MapSet.new([{0, 0, 3}, {1, 0, 3}])
    assert Store.outstanding_requests(<<>>, <<2>>) == MapSet.new([{0, 0, 3}])

    assert Store.received_block(<<>>, <<1>>, {0, 0, 3}) == :ok
    assert Store.outstanding_requests(<<>>) == MapSet.new([{0, 0, 3}, {1, 0, 3}])
    assert Store.outstanding_requests(<<>>, <<1>>) == MapSet.new([{1, 0, 3}])

    assert Store.received_block(<<>>, <<1>>, {1, 0, 3}) == :ok
    assert Store.received_block(<<>>, <<2>>, {0, 0, 3}) == :ok
    assert Store.outstanding_requests(<<>>) |> MapSet.size == 0
    assert Store.outstanding_requests(<<>>, <<1>>) |> MapSet.size == 0
  end

  test "set_missing_blocks/2 and pop_missing_block/2" do
    blocks = [{1, 0, 0}, {0, 0, 0}]
    assert Store.set_missing_blocks(<<>>, blocks) == :ok
    assert Store.pop_missing_block(<<>>, 0) == {0, 0, 0}
    assert Store.pop_missing_block(<<>>, 0) |> is_nil
    assert Store.pop_missing_block(<<>>, 1) == {1, 0, 0}
    assert Store.pop_missing_block(<<>>, 1) |> is_nil
  end
end
