defmodule Bittorrent.MessageTest do
  use ExUnit.Case

  test "parse request message" do
    buf = <<
      0x06,
      0x00, 0x00, 0x00, 0x01,
      0x00, 0x00, 0x00, 0x02,
      0x00, 0x00, 0x00, 0x03,
    >>
    {:ok, msg} = Bittorrent.Message.parse(buf)
    assert msg.index == 1
    assert msg.begin == 2
    assert msg.length == 3
  end

  test "parse piece message" do
    
    buf = <<
      0x07,
      0x00, 0x00, 0x00, 0x01,
      0x00, 0x00, 0x00, 0x02,
      0x00, 0x00, 0x00, 0x03,
    >>
    {:ok, msg} = Bittorrent.Message.parse(buf)
    assert msg.index == 1
    assert msg.begin == 2
    assert msg.block == <<0x00, 0x00, 0x00, 0x03>>
  end
end
