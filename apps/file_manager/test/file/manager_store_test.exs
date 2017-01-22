defmodule File.Manager.StoreTest do
  alias File.Manager.Store
  use ExUnit.Case

  setup do
    :ok = Store.reset

    info_hash = :crypto.strong_rand_bytes(20)
    files = [{"1.mp3", 5}, {"2.mp3", 11}]
    piece_hashes = 1..length(files)
                   |> Enum.map(fn _ -> :crypto.strong_rand_bytes(20) end)

    :ok = Store.add(info_hash, "/tmp/btd", files, piece_hashes, 3, 2)

    %{info_hash: info_hash, files: files, piece_hashes: piece_hashes}
  end

  test "blocks/2", %{info_hash: info_hash} do
    assert Store.blocks(info_hash, 0) == [
      {0, 2, :need},
      {2, 1, :need}
    ]
    assert Store.blocks(info_hash, 1) == [
      {0, 2, :need},
      {2, 1, :need}
    ]
    assert Store.blocks(info_hash, 2) == [
      {0, 2, :need},
      {2, 1, :need}
    ]
    assert Store.blocks(info_hash, 3) == [
      {0, 2, :need},
      {2, 1, :need}
    ]
    assert Store.blocks(info_hash, 4) == [
      {0, 2, :need},
      {2, 1, :need}
    ]
    assert Store.blocks(info_hash, 5) == [
      {0, 1, :need}
    ]
  end
  
  test "update_status/4", %{info_hash: info_hash} do
    assert Store.update_status(info_hash, 0, {0, 2}, :have) == :ok
    assert Store.blocks(info_hash, 0) == [
      {0, 2, :have},
      {2, 1, :need}
    ]
    assert Store.update_status(info_hash, 0, {2, 1}, :have) == :ok
    assert Store.blocks(info_hash, 0) == [
      {0, 2, :have},
      {2, 1, :have}
    ]
  end
  
  test "update_status/3", %{info_hash: info_hash} do
    assert Store.update_status(info_hash, 0, :have) == :ok
    assert Store.blocks(info_hash, 0) == [
      {0, 2, :have},
      {2, 1, :have}
    ]
  end
  
  test "piece_completed?/2", %{info_hash: info_hash} do
    refute Store.piece_completed?(info_hash, 0)
    :ok = Store.update_status(info_hash, 0, :have)
    assert Store.piece_completed?(info_hash, 0)
  end

  test "segments/4", %{info_hash: info_hash} do
    assert Store.segments(info_hash, 0, 0, 3) == [
      {"/tmp/btd/1.mp3", 0, 3}
    ]
    assert Store.segments(info_hash, 1, 0, 2) == [
      {"/tmp/btd/1.mp3", 3, 2}
    ]
    assert Store.segments(info_hash, 1, 0, 3) == [
      {"/tmp/btd/1.mp3", 3, 2},
      {"/tmp/btd/2.mp3", 0, 1}
    ]
  end

  test "files/1", %{info_hash: info_hash} do
    assert Store.files(info_hash) == [
      {"/tmp/btd/1.mp3", 5},
      {"/tmp/btd/2.mp3", 11}
    ]
  end
end
