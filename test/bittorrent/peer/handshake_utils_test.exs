defmodule Peer.HandshakeUtilsTest do
  use ExUnit.Case

  test "sync" do
    import Peer.HandshakeUtils, only: [sync: 2]

    assert sync(<<1, 2, 3>>, <<1, 2, 3>>) == <<>>
    assert sync(<<1, 2, 3>>, <<0, 1, 2, 3>>) == <<>>
    assert sync(<<1, 2, 3>>, <<0, 1, 2, 3, 4, 5>>) == <<4, 5>>
    assert sync(<<1, 2, 3>>, <<1>>) == <<1>>
  end
end

