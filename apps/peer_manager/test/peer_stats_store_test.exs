defmodule Peer.Stats.StoreTest do
  alias Peer.Stats
  alias Peer.Stats.Store
  use ExUnit.Case

  setup do
    :ok = Store.reset

    info_hash = :crypto.strong_rand_bytes(20)
    :ok = Store.register(info_hash)

    %{info_hash: info_hash}
  end

  test "register/1" do
    info_hash = :crypto.strong_rand_bytes(20)
    Store.register(info_hash)
    assert Store.stats(info_hash) == %Stats{}
  end
  
  test "deregister/1", %{info_hash: info_hash} do
    Store.deregister(info_hash)
    assert Store.stats(info_hash) == nil
  end

  test "incr_uploaded/2", %{info_hash: info_hash} do
    assert Store.incr_uploaded(info_hash, 5) == %Stats{uploaded: 5}
    assert Store.incr_uploaded(info_hash, 10) == %Stats{uploaded: 15}
  end
  
  test "incr_downloaded/2", %{info_hash: info_hash} do
    assert Store.incr_downloaded(info_hash, 5) == %Stats{downloaded: 5}
    assert Store.incr_downloaded(info_hash, 10) == %Stats{downloaded: 15}
  end
end
