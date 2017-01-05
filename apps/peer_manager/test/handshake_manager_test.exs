defmodule Peer.HandshakeTest do
  use ExUnit.Case

  @info_hash Application.fetch_env!(:peer_manager, :test_info_hash)

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
    {:ok, pid} = Peer.Handshake.start_link({127,0,0,1}, port, @info_hash)

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

    conn = %Peer.Socket{in_stream: ins, out_stream: outs, sock: ssock}
    {:ok, {conn, <<vc::bytes-size(8), _::32, lenpad::16>>}} = Peer.Socket.recv(conn, 14)
    assert vc == <<0::8*8>>

    if lenpad > 0 do
      {:ok, _} = :gen_tcp.recv(ssock, lenpad)
    end

    {:ok, {conn, <<ialen::16>>}} = Peer.Socket.recv(conn, 2)
    assert ialen == 49+19
    {:ok, {conn, <<pstrlen::8>>}} = Peer.Socket.recv(conn, 1)
    assert pstrlen == 19

    {:ok, {conn, pstr}} = Peer.Socket.recv(conn, pstrlen)
    assert pstr == "BitTorrent protocol"

    {:ok, {conn, reserved}} = Peer.Socket.recv(conn, 8)
    assert reserved == <<0::64>>

    {:ok, {conn, info_hash}} = Peer.Socket.recv(conn, 20)
    assert info_hash == @info_hash

    # TODO: assert peer id is correct
    {:ok, {conn, _}} = Peer.Socket.recv(conn, 20)

    Peer.Socket.send(conn, [
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
    assert Supervisor.count_children(Peer.Connection.Supervisor).workers == 1

    :gen_tcp.close(ssock)
  end

  test "incoming connection", ctx do
    Process.flag(:trap_exit, true)
    {:ok, pid} = Peer.Handshake.start_link(ctx.listen)

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

    conn = %Peer.Socket{in_stream: ins, out_stream: outs, sock: csock}

    Peer.Socket.send(conn, [
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

    {:ok, {conn, <<_::32, lenpad::16>>}} = Peer.Socket.recv(conn, 6)

    if lenpad > 0 do
      {:ok, _} = Peer.Socket.recv(csock, lenpad)
    end

    {:ok, {conn, <<pstrlen::8>>}} = Peer.Socket.recv(conn, 1)
    assert pstrlen == 19
    {:ok, {conn, <<pstr::bytes-size(19)>>}} = Peer.Socket.recv(conn, 19)
    assert pstr == "BitTorrent protocol"
    {:ok, {conn, <<reserved::bytes-size(8)>>}} = Peer.Socket.recv(conn, 8)
    assert reserved == <<0::64>>
    {:ok, {_, <<info_hash::bytes-size(20)>>}} = Peer.Socket.recv(conn, 20)
    assert info_hash == @info_hash


    assert_receive {:EXIT, ^pid, :normal}
    assert Supervisor.count_children(Peer.Connection.Supervisor).workers == 1

    :gen_tcp.close(csock)
  end
end
