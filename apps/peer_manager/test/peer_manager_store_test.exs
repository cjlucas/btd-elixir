defmodule Peer.Manager.StoreTest do
  alias Peer.Manager.Store
  use ExUnit.Case

  setup do
    :ok = Store.reset

    info_hash = :crypto.strong_rand_bytes(20)
    :ok = Store.add(info_hash)

    %{info_hash: info_hash, skey_hash: Peer.HandshakeUtils.req2(info_hash)}
  end

  test "add/1" do
    info_hash = :crypto.strong_rand_bytes(20)
    Store.add(info_hash)
    assert Store.stats(info_hash) == [uploaded: 0, downloaded: 0]
  end

  test "remove/1", %{info_hash: info_hash} do
    Store.remove(info_hash)
    assert Store.stats(info_hash) == nil
  end

  test "resolve_info_hash/1", %{info_hash: info_hash, skey_hash: skey_hash} do
    assert Store.resolve_info_hash(skey_hash) == info_hash
    assert Store.resolve_info_hash(<<>>) == nil
  end

  test "lookup_peer_id/1", %{info_hash: info_hash} do
    assert is_binary(Store.lookup_peer_id(info_hash))
    assert is_nil(Store.lookup_peer_id(<<>>))
  end

  test "incr_uploaded/2", %{info_hash: info_hash} do
    assert Store.incr_uploaded(info_hash, 5) == :ok
    assert Store.stats(info_hash) == [uploaded: 5, downloaded: 0]
    assert Store.incr_uploaded(info_hash, 10) == :ok
    assert Store.stats(info_hash) == [uploaded: 15, downloaded: 0]
  end

  test "incr_downloaded/2", %{info_hash: info_hash} do
    assert Store.incr_downloaded(info_hash, 5) == :ok
    assert Store.stats(info_hash) == [uploaded: 0, downloaded: 5]
    assert Store.incr_downloaded(info_hash, 10) == :ok
    assert Store.stats(info_hash) == [uploaded: 0, downloaded: 15]
  end
end
