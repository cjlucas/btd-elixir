defmodule Bittorrent.MessageTest do
  use ExUnit.Case
  import Bittorrent.Message
  alias Bittorrent.Message.{Request, Piece}

  test "parse request message" do
    buf = <<
      0x06,
      0x00, 0x00, 0x00, 0x01,
      0x00, 0x00, 0x00, 0x02,
      0x00, 0x00, 0x00, 0x03,
    >>
    msg = %Request{index: 1, begin: 2, length: 3}

    assert parse(buf) == {:ok, msg}
    assert encode(msg) == buf
  end

  test "parse piece message" do
    buf = <<
      0x07,
      0x00, 0x00, 0x00, 0x01,
      0x00, 0x00, 0x00, 0x02,
      0x00, 0x00, 0x00, 0x03,
    >>
    msg = %Piece{index: 1, begin: 2, block: <<0, 0, 0, 3>>}

    assert parse(buf) ==  {:ok, msg}
    assert parse(<<0x07, 0x00>>) == {:error, :decode_failed}

    assert encode(msg) == buf
  end
end
