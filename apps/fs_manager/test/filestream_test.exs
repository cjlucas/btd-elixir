defmodule Bittorrent.FilestreamTest do
  use ExUnit.Case
  import Bittorrent.FileStream, only: [items_in_range: 3]
  alias Bittorrent.FileStream.Item

  setup do

    files = [
      %Item{name: "1.mp3", offset: 0, size: 100},
      %Item{name: "2.mp3", offset: 100, size: 200},
      %Item{name: "3.mp3", offset: 300, size: 300},
    ]

    {:ok, %{files: files}}
  end

  test "when block is in single file", ctx do
    assert items_in_range(ctx.files, 0, 50) == [%Item{name: "1.mp3", offset: 0, size: 50}]
    assert items_in_range(ctx.files, 0, 100) == [%Item{name: "1.mp3", offset: 0, size: 100}]
  end

  test "when block spans multiple files", ctx do
    assert items_in_range(ctx.files, 50, 100) == [
     %Item{name: "1.mp3", offset: 50, size: 50},
     %Item{name: "2.mp3", offset: 0, size: 50}
   ]
  end
end
