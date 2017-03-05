defmodule Peer.Manager.NewStatsTest do
  alias Swarm.Stats
  use ExUnit.Case

  setup do
    {:ok, _} = Stats.start_link(<<>>)
    :ok
  end

  test "add_peers/2 and pop_peer/1 with single peer" do
    # test single peer
    assert Stats.pop_peer(<<>>) |> is_nil
    assert Stats.add_peers(<<>>, [{"127.0.0.1", 50000}]) == :ok
    assert Stats.pop_peer(<<>>) == {"127.0.0.1", 50000}
    assert Stats.pop_peer(<<>>) |> is_nil
  end

  test "add_peers/2 and pop_peer/1 with multiple peers" do
    assert Stats.add_peers(<<>>, [{"127.0.0.1", 50000}, {"8.8.8.8", 50000}]) == :ok
    refute Stats.pop_peer(<<>>) |> is_nil
    refute Stats.pop_peer(<<>>) |> is_nil
    assert Stats.pop_peer(<<>>) |> is_nil
  end

  test "add_peers/2 and pop_peer/1 with duplicate peers" do
    assert Stats.add_peers(<<>>, [{"127.0.0.1", 50000}, {"127.0.0.1", 50000}]) == :ok
    refute Stats.pop_peer(<<>>) |> is_nil
    assert Stats.pop_peer(<<>>) |> is_nil
  end

  test "peer_id/1" do
    assert Stats.peer_id(<<>>) |> is_binary
  end

  test "incr_uploaded/3 and incr_downloaded/3 and stats/1" do
    assert Stats.incr_uploaded(<<>>, 5) == :ok
    assert Stats.stats(<<>>) == [uploaded: 5, downloaded: 0]

    assert Stats.incr_uploaded(<<>>, 10) == :ok
    assert Stats.stats(<<>>) == [uploaded: 15, downloaded: 0]

    assert Stats.incr_downloaded(<<>>, 5) == :ok
    assert Stats.stats(<<>>) == [uploaded: 15, downloaded: 5]

    assert Stats.incr_downloaded(<<>>, 10) == :ok
    assert Stats.stats(<<>>) == [uploaded: 15, downloaded: 15]
  end
end
