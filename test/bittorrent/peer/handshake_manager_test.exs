defmodule MockTorrentStore do
  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: Torrent.Store)
  end

  def lookup(:info_hash, hash), do: Torrent.Store.call({:lookup, :info_hash, hash})

  def init(:ok) do
    {:ok, nil}
  end

  def handle_call({:lookup, :info_hash, hash}, _from, state) do
    {:reply, {:ok, %Torrent{peer_id: <<0::20*8>>}}, state}
  end
end

defmodule Peer.HandshakeTest do
  use ExUnit.Case

  test "sync" do
    assert Peer.Handshake.sync(<<1, 2, 3>>, <<1, 2, 3>>) == <<>>
    assert Peer.Handshake.sync(<<1, 2, 3>>, <<0, 1, 2, 3>>) == <<>>
    assert Peer.Handshake.sync(<<1, 2, 3>>, <<0, 1, 2, 3, 4, 5>>) == <<4, 5>>
    assert Peer.Handshake.sync(<<1, 2, 3>>, <<1>>) == <<1>>
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

  def sync(sock, needle, data \\ <<>>) do
    case :gen_tcp.recv(sock, 1, 1000) do
      {:ok, buf} ->
        data = data <> buf
        rest = Peer.Handshake.sync(needle, data)
        if byte_size(data) > byte_size(rest) do
          rest
        else
          sync(sock, needle, data)
        end
      {:error, _} ->
        :error
    end
  end

  test "outgoing connection", ctx do
    {:ok, port} = :inet.port(ctx.listen)
    {:ok, pid} = Peer.HandshakeManager.start_link({127,0,0,1}, port, @info_hash)
    {:ok, _} = MockTorrentStore.start_link

    Process.flag(:trap_exit, true)
    
    {:ok, ssock} = :gen_tcp.accept(ctx.listen)

    {:ok, pubA} = :gen_tcp.recv(ssock, 96)
    
    {priv, pub} = Peer.Handshake.gen_keys()
    :ok = :gen_tcp.send(ssock, [pub, <<0::16>>])

    s = Peer.Handshake.calc_secret(pubA, priv)
    assert sync(ssock, Peer.Handshake.req1(s)) == <<>>

    {:ok, req23} = :gen_tcp.recv(ssock, 20)
    req2 = Peer.Handshake.bin_xor(Peer.Handshake.req3(s), req23)
    assert Peer.Handshake.req2(@info_hash) == req2

    {ins, outs} = Peer.Handshake.init_streams(
      Peer.Handshake.key(<<"keyA">>, s, @info_hash),
      Peer.Handshake.key(<<"keyB">>, s, @info_hash)
    )

    conn = %Peer.HandshakeManager.Connection{in_stream: ins, out_stream: outs, sock: ssock}
    {:ok, {conn, << vc::bytes-size(8), crypto_provide::32, lenpad::16>>}} = Peer.HandshakeManager.Connection.recv(conn, 14)
    assert vc == <<0::8*8>>

    if lenpad > 0 do
      {:ok, _} = :gen_tcp.recv(ssock, lenpad)
    end

    {:ok, {conn, <<ialen::16>>}} = Peer.HandshakeManager.Connection.recv(conn, 2)
    assert ialen == 49+19
    {:ok, {conn, <<pstrlen::8>>}} = Peer.HandshakeManager.Connection.recv(conn, 1)
    assert pstrlen == 19

    {:ok, {conn, pstr}} = Peer.HandshakeManager.Connection.recv(conn, pstrlen)
    assert pstr == "BitTorrent protocol"

    {:ok, {conn, reserved}} = Peer.HandshakeManager.Connection.recv(conn, 8)
    assert reserved == <<0::64>>

    {:ok, {conn, info_hash}} = Peer.HandshakeManager.Connection.recv(conn, 20)
    assert info_hash == @info_hash

    # TODO: assert peer id is correct
    {:ok, {conn, _}} = Peer.HandshakeManager.Connection.recv(conn, 20)


    Peer.HandshakeManager.Connection.send(conn, [
      <<0::64>>,
      <<3::32>>,
      <<0::16>>,
      <<19::8>>,
      "BitTorrent protocol",
      <<0::64>>,
      @info_hash,
      <<0::20*8>>
    ])

    assert_receive {:EXIT, ^pid, :normal}
  end
end
