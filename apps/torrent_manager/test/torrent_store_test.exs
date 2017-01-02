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
               |> List.first

    {:ok, %{torrent: torrent}}
  end

  test "start_link", %{torrent: torrent} do
    assert {:ok, pid} = Torrent.Store.start_link(torrent)
    assert is_pid(pid)
  end
end
