defmodule MockTorrentStore do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: Torrent.Store)
  end

  def lookup(:info_hash, info_hash) do
    IO.puts("OMGWAS CALLED")
    {:ok, %Torrent{peer_id: <<0::20*8>>}}
  end

  def init(:ok) do
    {:ok, nil}
  end
end

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
    {:ok, _} = Peer.HandshakeManager.start_link
    {:ok, _} = MockTorrentStore.start_link

    {:ok, port} = :inet.port(ctx.listen)
    {:ok, csock} = :gen_tcp.connect({127,0,0,1}, port, [:binary, active: true])
    {:ok, ssock} = :gen_tcp.accept(ctx.listen)

    assert Peer.HandshakeManager.register_outgoing_peer(csock, @info_hash) == :ok

    {:ok, << pubA::bytes-size(96), data::binary>>} = :gen_tcp.recv(ssock, 0)
    IO.puts("pubA = #{inspect pubA}")
    
    {priv, pub} = Peer.Handshake.gen_keys()
    IO.puts("OMGHERE #{inspect pub}")
    :ok = :gen_tcp.send(ssock, [pub, <<0::16>>])

    s = Peer.Handshake.calc_secret(pubA, priv)
    IO.puts("DA S = #{inspect s}")

    data = with {:ok, buf} <- :gen_tcp.recv(ssock, 0), do: data <> buf
    IO.puts("got data #{byte_size(data)}")
    rest = Peer.Handshake.sync(Peer.Handshake.req1(s), data)
    assert byte_size(rest) > 0
    data = rest

    << req23::bytes-size(20), data::binary >> = data
    req2 = Peer.Handshake.bin_xor(Peer.Handshake.req3(s), req23)
    assert Peer.Handshake.req2(@info_hash) == req2

    {ins, outs} = Peer.Handshake.init_streams(
      Peer.Handshake.key(<<"keyA">>, s, @info_hash),
      Peer.Handshake.key(<<"keyB">>, s, @info_hash)
    )

    conn = %Peer.HandshakeManager.Connection{in_stream: ins, out_stream: outs, sock: ssock}
    {:ok, {conn, << vc::bytes-size(8), crypto_provide::32, lenpad::8, data::binary>>}} = Peer.HandshakeManager.Connection.recv(conn, 0)
    assert vc == <<0::64>>

    <<pad::bytes-size(lenpad), data::binary>> = data
    <<pstrlen::8, data::binary>> = data
    assert pstrlen == 19
  end
end
