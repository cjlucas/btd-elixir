defmodule Torrent.RegistryTest do
  use ExUnit.Case

  @fixtures_dir Path.join(__DIR__, "fixtures")

  setup do
    Torrent.Registry.reset
    
    {:ok, t} = @fixtures_dir
      |> Path.join("ubuntu-16.10-desktop-amd64.iso.torrent")
      |> File.read!
      |> Torrent.parse
    
    {:ok, [torrent: t]}
  end

  test "register", %{torrent: t} do
    {:ok, {pid1, pid2}} = Torrent.Registry.register(t)
    assert Torrent.Registry.size == 1

    assert [Torrent.Store.Supervisor, Torrent.Manager.Supervisor]
    |> Enum.map(&Supervisor.which_children(&1))
    |> Enum.map(&List.first/1)
    |> Enum.map(&elem(&1, 1)) == [pid1, pid2]
  end
  
  test "deregister", %{torrent: t} do
    skey_hash = :crypto.hash(:sha, [<<"req2">>, t.info_hash])
    {:ok, _} = Torrent.Registry.register(t)
    assert Torrent.Registry.deregister(t.info_hash) == :ok
    
    assert [Torrent.Store.Supervisor, Torrent.Manager.Supervisor]
    |> Enum.map(&Supervisor.which_children(&1))
    |> Enum.map(&length/1) == [0, 0]
    
    assert Torrent.Registry.size == 0
    assert Torrent.Registry.lookup({:info_hash, t.info_hash}) == {:error, :not_found}
    assert Torrent.Registry.lookup({:skey_hash, skey_hash}) == {:error, :not_found}
  end

  test "lookup", %{torrent: t} do
    skey_hash = :crypto.hash(:sha, [<<"req2">>, t.info_hash])
    {:ok, {pid1, pid2}} = Torrent.Registry.register(t)

    assert Torrent.Registry.lookup({:info_hash, t.info_hash}) == {:ok, {pid1, pid2}}
    assert Torrent.Registry.lookup({:skey_hash, skey_hash}) == {:ok, {pid1, pid2}}
  end
end
