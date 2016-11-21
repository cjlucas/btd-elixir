defmodule Peer.HandshakeManagerTest do
  use ExUnit.Case

  @info_hash <<
    0x94, 0x11, 0x2C, 0x62, 0x5A,
    0x02, 0xBA, 0x80, 0x5A, 0xD0,
    0xE0, 0x92, 0x87, 0xF4, 0xA7,
    0xFC, 0xEF, 0xA8, 0x29, 0x1B
  >>

  setup do
    {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false])

    on_exit fn -> 
      :gen_tcp.close(listen)
    end

    {:ok, %{listen: listen}}
  end

  test "outgoing connection", ctx do
    :ok = Peer.HandshakeManager.start_link
    :ok = Torrent.Store.start_link

    {:ok, port} = :inet.port(ctx.listen)
    {:ok, csock} = :gen_tcp.connect({127,0,0,1}, port, [:binary, active: true])
    {:ok, ssock} = :gen_tcp.accept(ctx.listen)


    assert :ok == Peer.HandshakeManager.register_outgoing_peer(csock, @info_hash)
  end
end
