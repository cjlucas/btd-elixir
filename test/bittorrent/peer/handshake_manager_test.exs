defmodule MockTorrentStore do
  use GenServer

  defmodule State do
    defstruct req2_map: Map.new, hashes: MapSet.new
  end

  def start_link(hashes) do
    GenServer.start_link(__MODULE__, hashes, name: Torrent.Store)
  end

  def lookup(:info_hash, hash), do: Torrent.Store.call({:lookup, :info_hash, hash})
  def lookup(:skey_hash, hash), do: Torrent.Store.call({:lookup, :skey_hash, hash})

  def init(hashes) do
    map = for h <- hashes, into: %{}, do: {Peer.HandshakeUtils.req2(h), h}
    hashes = for h <- hashes, into: MapSet.new, do: h
    {:ok, %State{req2_map: map, hashes: hashes}}
  end

  def handle_call({:lookup, :skey_hash, hash}, _from, state) do
    if Map.has_key?(state.req2_map, hash) do
      {:reply, {:ok, %Torrent{info_hash: state.req2_map[hash], peer_id: <<0::20*8>>}}, state}
    else
      {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:lookup, :info_hash, hash}, _from, state) do
    if MapSet.member?(state.hashes, hash) do
      {:reply, {:ok, %Torrent{info_hash: hash, peer_id: <<0::20*8>>}}, state}
    else
      {:reply, {:error, :not_found}, state}
    end
  end
end

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
        rest = Peer.HandshakeUtils.sync(needle, data)
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
    {:ok, _} = MockTorrentStore.start_link([@info_hash])

    Process.flag(:trap_exit, true)
    
    {:ok, ssock} = :gen_tcp.accept(ctx.listen)

    {:ok, pubA} = :gen_tcp.recv(ssock, 96)
    
    {priv, pub} = Peer.HandshakeUtils.gen_keys()
    :ok = :gen_tcp.send(ssock, [pub, <<0::16>>])

    s = Peer.HandshakeUtils.calc_secret(pubA, priv)
    assert sync(ssock, Peer.HandshakeUtils.req1(s)) == <<>>

    {:ok, req23} = :gen_tcp.recv(ssock, 20)
    req2 = Peer.HandshakeUtils.bin_xor(Peer.HandshakeUtils.req3(s), req23)
    assert Peer.HandshakeUtils.req2(@info_hash) == req2

    {ins, outs} = Peer.HandshakeUtils.init_streams(
      Peer.HandshakeUtils.key(<<"keyA">>, s, @info_hash),
      Peer.HandshakeUtils.key(<<"keyB">>, s, @info_hash)
    )

    conn = %Peer.HandshakeManager.Connection{in_stream: ins, out_stream: outs, sock: ssock}
    {:ok, {conn, <<vc::bytes-size(8), _::32, lenpad::16>>}} = Peer.HandshakeManager.Connection.recv(conn, 14)
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

    :gen_tcp.close(ssock)
  end

  test "incoming connection", ctx do
    Process.flag(:trap_exit, true)
    {:ok, pid} = Peer.HandshakeManager.start_link(ctx.listen)
    {:ok, _} = MockTorrentStore.start_link([@info_hash])

    {:ok, port} = :inet.port(ctx.listen)
    {:ok, csock} = :gen_tcp.connect({127,0,0,1}, port, [:binary, active: false])

    {priv, pub} = Peer.HandshakeUtils.gen_keys()
    :ok = :gen_tcp.send(csock, [pub, <<0::16>>])
    
    {:ok, pubB} = :gen_tcp.recv(csock, 96)

    s = Peer.HandshakeUtils.calc_secret(pubB, priv)

    :ok = :gen_tcp.send(csock, [
      Peer.HandshakeUtils.req1(s),
      Peer.HandshakeUtils.bin_xor(
        Peer.HandshakeUtils.req2(@info_hash),
        Peer.HandshakeUtils.req3(s)
      )
    ])
    
    {ins, outs} = Peer.HandshakeUtils.init_streams(
      Peer.HandshakeUtils.key(<<"keyB">>, s, @info_hash),
      Peer.HandshakeUtils.key(<<"keyA">>, s, @info_hash)
    )

    conn = %Peer.HandshakeManager.Connection{in_stream: ins, out_stream: outs, sock: csock}

    Peer.HandshakeManager.Connection.send(conn, [
      <<0::64>>, # vc
      <<3::32>>, # crypto_provide
      <<4::16>>, # len(pad)
      <<0::32>>, # pad
      <<68::16>>, # len(ia)
      <<19::8>>, # ia
      "BitTorrent protocol",
      <<0::64>>,
      @info_hash,
      <<0::20*8>>
    ])

    {ins, expected_vc} = :crypto.stream_encrypt(ins, <<0::64>>)
    conn = %{conn | in_stream: ins}

    assert sync(csock, expected_vc) == <<>>

    {:ok, {conn, <<_::32, lenpad::16>>}} = Peer.HandshakeManager.Connection.recv(conn, 6)

    if lenpad > 0 do
      {:ok, _} = Peer.HandshakeManager.Connection.recv(csock, lenpad)
    end

    {:ok, {conn, <<pstrlen::8>>}} = Peer.HandshakeManager.Connection.recv(conn, 1)
    assert pstrlen == 19
    {:ok, {conn, <<pstr::bytes-size(19)>>}} = Peer.HandshakeManager.Connection.recv(conn, 19)
    assert pstr == "BitTorrent protocol"
    {:ok, {conn, <<reserved::bytes-size(8)>>}} = Peer.HandshakeManager.Connection.recv(conn, 8)
    assert reserved == <<0::64>>
    {:ok, {conn, <<info_hash::bytes-size(20)>>}} = Peer.HandshakeManager.Connection.recv(conn, 20)
    assert info_hash == @info_hash

    assert_receive {:EXIT, ^pid, :normal}

    #:gen_tcp.close(csock)
  end
end
