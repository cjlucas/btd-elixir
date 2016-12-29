defmodule Torrent.StoreTest do
  import HelperSigils
  use ExUnit.Case

  @fixtures_dir Path.join(__DIR__, "fixtures")

  setup_all do
    {:ok, files} = File.ls(@fixtures_dir)
    torrents = files
               |> Enum.filter(&(String.ends_with?(&1, ".torrent")))
               |> Enum.map(&(Path.join(@fixtures_dir, &1)))
               |> Enum.map(&File.read!/1)
               |> Enum.map(&Bento.torrent!/1)


    {:ok, %{torrents: torrents}}
  end

  test "register", ctx do
    {:ok, _} = Torrent.Store.start_link

    assert Torrent.Store.register(List.first(ctx.torrents), "/") == :ok
    assert length(Torrent.Store.torrents) == 1
  end

  test "lookup by info_hash", ctx do
    {:ok, _} = Torrent.Store.start_link

    :ok = Torrent.Store.register(List.first(ctx.torrents), "/")

    hash = ~h(0403fb4728bd788fbcb67e87d6feb241ef38c75a)
    {key, val} = Torrent.Store.lookup(:info_hash, hash)
    assert key == :ok
    assert val.info_hash == hash
  end
end
