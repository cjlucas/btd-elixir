defmodule TorrentTest do
  import HelperSigils
  use ExUnit.Case

  @fixtures_dir Path.join(__DIR__, "fixtures")

  test "parse" do
    {:ok, t} = @fixtures_dir
      |> Path.join("ubuntu-16.10-desktop-amd64.iso.torrent")
      |> File.read!
      |> Torrent.parse
    assert t.info_hash == ~h(0403fb4728bd788fbcb67e87d6feb241ef38c75a)
  end
end