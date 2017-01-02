defmodule Torrent.RegistryTest do
  use ExUnit.Case

  setup do
    Torrent.Registry.reset
  end

  test "register" do
    info_hash = <<0::20*8>>
    assert Torrent.Registry.register(info_hash, self(), self()) == :ok
    assert Torrent.Registry.size == 1
  end
  
  test "deregister" do
    info_hash = <<0::20*8>>
    assert Torrent.Registry.register(info_hash, self(), self()) == :ok
    assert Torrent.Registry.deregister(info_hash) == :ok
    assert Torrent.Registry.size == 0
  end

  test "lookup" do
    info_hash = <<0::20*8>>
    skey_hash = :crypto.hash(:sha, [<<"req2">>, info_hash])
    assert Torrent.Registry.register(info_hash, self(), self()) == :ok
    assert Torrent.Registry.lookup({:info_hash, info_hash}) == {:ok, {self(), self()}}
    assert Torrent.Registry.lookup({:skey_hash, skey_hash}) == {:ok, {self(), self()}}
  end
end
