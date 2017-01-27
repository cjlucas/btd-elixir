defmodule Torrent.StoreTest do
  import HelperSigils
  use ExUnit.Case

  @fixtures_dir Path.join(__DIR__, "fixtures")

  setup_all do
    {:ok, files} = File.ls(@fixtures_dir)
    torrent = files
               |> Enum.filter(&(String.ends_with?(&1, ".torrent")))
               |> Enum.map(&(Path.join(@fixtures_dir, &1)))
               |> Enum.map(&File.read!/1)
               |> Enum.map(&Torrent.parse/1)
               |> Enum.filter(&(elem(&1, 0) == :ok))
               |> Enum.map(&elem(&1, 1))
               |> List.first


    {:ok, %{torrent: torrent}}
  end

  test "add/1, get/1 and remove/1", %{torrent: torrent} do
    assert Torrent.Store.add(torrent) == :ok
    assert Torrent.Store.get(torrent.info_hash) == torrent
    assert Torrent.Store.remove(torrent.info_hash) == :ok
    assert Torrent.Store.get(torrent.info_hash) == nil
  end
end
