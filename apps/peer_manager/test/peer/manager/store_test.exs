defmodule Peer.Manager.NewStoreTest do
  alias Peer.Manager.Store
  use ExUnit.Case

  setup do
    {:ok, _} = Store.start_link(<<>>)
    :ok
  end

  test "add_peer/2 and has_peer?/2" do
    refute Store.has_peer?(<<>>, <<1>>)
    assert Store.add_peer(<<>>, <<1>>) == :ok
    assert Store.has_peer?(<<>>, <<1>>)
  end

  test "peer_id/1" do
    assert is_binary(Store.peer_id(<<>>))
  end

  test "incr_uploaded/3 and incr_downloaded/3 and stats/1" do
    :ok = Store.add_peer(<<>>, <<1>>)

    assert Store.incr_uploaded(<<>>, <<1>>, 5) == :ok
    assert Store.stats(<<>>) == [uploaded: 5, downloaded: 0]

    assert Store.incr_uploaded(<<>>, <<1>>, 10) == :ok
    assert Store.stats(<<>>) == [uploaded: 15, downloaded: 0]

    assert Store.incr_downloaded(<<>>, <<1>>, 5) == :ok
    assert Store.stats(<<>>) == [uploaded: 15, downloaded: 5]

    assert Store.incr_downloaded(<<>>, <<1>>, 10) == :ok
    assert Store.stats(<<>>) == [uploaded: 15, downloaded: 15]
  end
end
