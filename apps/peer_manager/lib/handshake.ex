defmodule Peer.Handshake do
  import Peer.HandshakeUtils
  import :erlang, only: [iolist_size: 1, iolist_to_binary: 1]
  use GenServer
  require Logger

  @torrent_info_provider Application.fetch_env!(:peer_manager, :torrent_info_provider)

  @name __MODULE__
  
  @vc <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>
  @crypto_provide <<2::32>> # rc4 only

  @pstr "BitTorrent protocol"
  @pstrlen 19

  # TODO: this should be in external config
  @sock_timeout 1000

  @outgoing_flow [
    :send_pubkey,
    :recv_pubkey,
    :calc_secret,
    :send_reqs,
    :init_streams,
    :send_vc,
    :send_crypto_provide,
    :send_pad,
    :send_ia,
    :recv_vc,
    :recv_crypto_select,
    :recv_pad,
    :recv_pstrlen,
    :recv_pstr,
    :recv_reserved,
    :recv_info_hash,
    :recv_peer_id,
  ]

  @incoming_flow [
    :recv_pubkey,
    :send_pubkey,
    :calc_secret,
    :recv_reqs,
    :init_streams,
    :recv_vc,
    :recv_crypto_provide,
    :recv_pad,
    :recv_ialen,
    :recv_pstrlen,
    :recv_pstr,
    :recv_reserved,
    :recv_info_hash,
    :recv_peer_id,
    :send_vc,
    :send_crypto_provide, # TODO: implement send_crypto_select
    :send_pad,
    :send_handshake,
  ]

  defmodule InitialState do
    defstruct lsock: nil, host: nil, port: nil, info_hash: <<>>
  end
  
  defmodule State do
    defstruct incoming: false,
      conn: nil,
      states: [],
      buffer: [],
      info_hash: <<>>,
      peer_id: <<>>, # their id
      pubkey: <<>>, # their pubkey
      privkey: <<>>, # our privkey
      secret: <<>>,  # S
      crypto_select: nil,
      expected_vc: nil # the encrypted vc to sync on
  end

  def start_link(sock) do
    GenServer.start_link(@name, {:listen, sock})
  end
  
  def start_link(host, port, info_hash) do
    GenServer.start_link(@name, {:connect, host, port, info_hash})
  end

  defp trigger_handler, do: GenServer.cast(self(), :trigger_handler)

  def handle_cast(:trigger_handler, state), do: dispatch_handler(state)

  def init({:listen, sock}) do
    {:ok, %InitialState{lsock: sock}, 0}
  end

  def init({:connect, host, port, info_hash}) do
    {:ok, %InitialState{host: host, port: port, info_hash: info_hash}, 0}
  end

  def handle_info(:timeout, %InitialState{lsock: lsock} = state) when not is_nil(lsock) do
    case :gen_tcp.accept(lsock) do
      {:ok, sock} ->
        :inet.setopts(sock, [active: true])
        conn = %Peer.Socket{sock: sock}
        Peer.Handshake.Supervisor.listen(lsock)
        {:noreply, %State{incoming: true, states: @incoming_flow, conn: conn}}
      {:error, _} ->
        {:stop, :normal, state}
    end
  end
  
  def handle_info(:timeout, %InitialState{host: host, port: port, info_hash: hash} = state) do
    host = host
           |> String.split(".")
           |> Enum.map(&String.to_integer/1)
           |> List.to_tuple

    case :gen_tcp.connect(host, port, [:binary, active: true]) do
      {:ok, sock} ->
        conn = %Peer.Socket{sock: sock}
        trigger_handler()
        {:noreply, %State{incoming: false, states: @outgoing_flow, info_hash: hash, conn: conn}}
      {:error, _} ->
        {:stop, :normal, state}
    end
  end

  def handle_info({:tcp, _sock, data}, state) do
    trigger_handler()
    {:noreply, update_in(state.buffer, &([&1, data]))}
  end

  def handle_info({:tcp_closed, _sock}, _state) do
    Logger.debug("in tcp_closed")
    {:stop, :closed}
  end

  # state handlers
  
  defp dispatch_handler(%{conn: conn, info_hash: h, peer_id: id, buffer: buf, states: []} = state) do
    Logger.debug("All done")
    with :ok <- :inet.setopts(conn.sock, [active: false]),
      {:ok, pid} <- Peer.Connection.Supervisor.start_child(h, conn),
      :ok <- :inet.setopts(conn.sock, [active: true]) do
        send(pid, {:tcp, conn.sock, iolist_to_binary(buf)})
        :gen_tcp.controlling_process(conn.sock, pid)
        {:stop, :normal, state}
    else
      {:error, reason} ->
        Logger.debug("Error occured while transferring socket: #{inspect reason}")
        {:stop, :kill, state}
    end
  end
  defp dispatch_handler(%{states: [cur_state | rem_states]} = state) do
    Logger.debug("cur_state = #{cur_state}")
    case handle_state(cur_state, state) do
      {:next_state, info} ->
        trigger_handler() # queue the next state handler
        {:noreply, %{info | states: rem_states}}
      :no_change ->
        {:noreply, state}
      {:error, reason} ->
        # TODO: proper error handling
        Logger.debug("handle_state received error (state = #{cur_state}, reason = #{reason})")
        {:noreply, state}
    end
  end
  
  defp handle_state(:send_pubkey, %{conn: conn} = info) do
    {privkey, pubkey} = gen_keys()
    pad = :crypto.strong_rand_bytes(rand(512))

    case Peer.Socket.send(conn, [pubkey, pad]) do
      {:ok, conn} ->
        {:next_state, %{info | privkey: privkey, conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_pubkey, %{buffer: buf} = info) do
    if iolist_size(buf) >= 96 do
      << pubkey :: bytes-size(96), rest :: binary >> = iolist_to_binary(buf)
      {:next_state, %{info | pubkey: pubkey, buffer: [rest]}}
    else
      :no_change
    end
  end
  
  defp handle_state(:calc_secret, %{pubkey: pub, privkey: priv} = info) do
    Logger.debug("calc_secret S = #{inspect calc_secret(pub, priv)}")
    {:next_state, %{info | secret: calc_secret(pub, priv)}}
  end

  defp handle_state(:send_reqs, %{conn: conn, secret: s, info_hash: info_hash} = info) do
    payload = [
      hash([<<"req1">>, s]),
      bin_xor(hash([<<"req2">>, info_hash]), hash([<<"req3">>, s]))
    ]

    case Peer.Socket.send(conn, payload) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_reqs, %{secret: s, buffer: buf} = info) do
    if iolist_size(buf) >= 40 do
      buf = iolist_to_binary(buf)
      synced_buf = sync(req1(s), buf)
      if buf == synced_buf do
        :no_change
      else
        # FIXME: we're not guaranteed at least 20 bytes here
        <<
          req23buf :: bytes-size(20),
          rest :: binary
        >> = synced_buf

        skey_hash = bin_xor(req23buf, req3(s))
        case @torrent_info_provider.resolve_info_hash(skey_hash) do
          {:ok, info_hash} ->
            Logger.debug("GOT TORRENT! #{inspect info_hash}")
            {:next_state, %{info | info_hash: info_hash, buffer: [rest]}}
          {:error, reason} ->
            Logger.debug("Resolve info hash failed (reason = #{reason})")
            {:error, :resolve_info_hash_failed}
        end
      end
    else
      :no_change
    end
  end

  defp handle_state(:init_streams, %{incoming: true, conn: conn, secret: s, info_hash: hash} = info) do
    {ins, outs} = init_streams(key(<<"keyA">>, s, hash), key(<<"keyB">>, s, hash))
    {:next_state, put_in(info.conn, %{conn | in_stream: ins, out_stream: outs})}
  end
  
  defp handle_state(:init_streams, %{incoming: false, conn: conn, secret: s, info_hash: hash} = info) do
    {ins, outs} = init_streams(key(<<"keyB">>, s, hash), key(<<"keyA">>, s, hash))
    {:next_state, put_in(info.conn, %{conn | in_stream: ins, out_stream: outs})}
  end

  defp handle_state(:send_vc, %{conn: conn} = info) do
    case Peer.Socket.send(conn, @vc) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_vc, %{conn: conn, expected_vc: vc} = info) when is_nil(vc) do
    {stream, vc} = :crypto.stream_encrypt(conn.in_stream, @vc)
    conn = %{conn | in_stream: stream}
    handle_state(:recv_vc, %{info | expected_vc: vc, conn: conn})
  end
  
  defp handle_state(:recv_vc, %{expected_vc: vc, buffer: buf} = info) do
    if iolist_size(buf) >= byte_size(vc) do
      Logger.debug("recv_vc")
      Logger.debug(inspect vc)
      Logger.debug(inspect buf)
      rest = sync(vc, iolist_to_binary(buf))
      Logger.debug("rest = #{inspect rest}")
      if byte_size(rest) > 0 do
        {:next_state, %{info | buffer: [rest]}}
      else
        :no_change
      end
    else
      :no_change
    end
  end

  defp handle_state(:recv_crypto_select, %{conn: conn, buffer: buf} = info) do
    if iolist_size(buf) >= byte_size(@crypto_provide) do
      << cs_enc::bytes-size(4), rest::binary>> = iolist_to_binary(buf)
      {conn, cs} = Peer.Socket.decrypt(conn, cs_enc)
      Logger.debug("crypto_select: #{inspect cs}")
      {:next_state, %{info | conn: conn, crypto_select: cs, buffer: [rest]}}
    else
      :no_change
    end
  end
  
  defp handle_state(:recv_crypto_provide, %{conn: conn, buffer: buf} = info) do
    if iolist_size(buf) >= byte_size(@crypto_provide) do
      << cp_enc::bytes-size(4), rest::binary>> = iolist_to_binary(buf)
      {conn, cp} = Peer.Socket.decrypt(conn, cp_enc)
      Logger.debug("cp = #{inspect cp}")
      {:next_state, %{info | conn: conn, crypto_select: cp, buffer: [rest]}}
    else
      :no_change
    end
  end
  
  defp handle_state(:send_crypto_provide, %{conn: conn} = info) do
    case Peer.Socket.send(conn, @crypto_provide) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_pad, %{buffer: buf}) when byte_size(buf) < 2 do
    :no_change
  end
  defp handle_state(:recv_pad, %{conn: conn, buffer: buf} = info) do
    Logger.debug(iolist_size(buf))
    << enc_len::bytes-size(2), rest::binary >> = iolist_to_binary(buf)
    {conn, <<len::16>>} = Peer.Socket.decrypt(conn, enc_len)
    Logger.debug(len)
    case rest do
      << pad::bytes-size(len), rest::binary>> ->
        {conn, _} = Peer.Socket.decrypt(conn, pad)
        {:next_state, %{info | conn: conn, buffer: [rest]}}
      _ ->
        :no_change
    end
  end

  defp handle_state(:send_pad, %{conn: conn} = info) do
    # only send len(padX)
    case Peer.Socket.send(conn, <<0x00, 0x00>>) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end
  
  defp handle_state(:recv_ialen, %{conn: conn, buffer: buf} = info) do
    ialen = 49 + @pstrlen
    if iolist_size(buf) >= 2 do
      <<ialen_enc::bytes-size(2), rest::binary>> = iolist_to_binary(buf)
      {conn, <<ialen::16>>} = Peer.Socket.decrypt(conn, ialen_enc)
      Logger.debug("recv_ialen (ialen = #{ialen})")
      {:next_state, %{info | conn: conn, buffer: [rest]}}
    else
      :no_change
    end
  end

  defp handle_state(:send_ia, %{conn: conn} = info) do
    ialen = 49 + @pstrlen
    case Peer.Socket.send(conn, <<ialen::16>>) do
      {:ok, conn} ->
        handle_state(:send_handshake, %{info | conn: conn})
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:send_handshake, %{conn: conn, info_hash: info_hash} = info) do
    case @torrent_info_provider.resolve_peer_id(info_hash) do
      {:ok, peer_id} ->
        payload = [
          <<@pstrlen::8>>,
          @pstr,
          <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>,
          info_hash,
          peer_id,
        ]
        case Peer.Socket.send(conn, payload) do
          {:ok, conn} ->
            {:next_state, %{info | conn: conn}}
          {:error, reason} ->
            {:error, reason}
        end
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_pstrlen, %{conn: conn, buffer: buf} = info) do
    if iolist_size(buf) >= 1 do
      <<pstrlen_enc::bytes-size(1), rest::binary>> = iolist_to_binary(buf)
      {conn, <<pstrlen::8>>} = Peer.Socket.decrypt(conn, pstrlen_enc)
      if pstrlen == @pstrlen do
        {:next_state, %{info | conn: conn, buffer: [rest]}}
      else
        {:error, :bad_pstrlen}
      end
    else
      :no_change
    end
  end
  
  defp handle_state(:recv_pstr, %{conn: conn, buffer: buf} = info) do
    if iolist_size(buf) >= @pstrlen do
      <<pstr_enc::bytes-size(@pstrlen), rest::binary>> = iolist_to_binary(buf)
      {conn, pstr} = Peer.Socket.decrypt(conn, pstr_enc)
      if pstr == @pstr do
        {:next_state, %{info | conn: conn, buffer: [rest]}}
      else
        {:error, :bad_pstr}
      end
    else
      :no_change
    end
  end
  
  defp handle_state(:recv_reserved, %{conn: conn, buffer: buf} = info) do
    if iolist_size(buf) >= 8 do
      <<reserved_enc::bytes-size(8), rest::binary>> = iolist_to_binary(buf)
      {conn, reserved} = Peer.Socket.decrypt(conn, reserved_enc)
      {:next_state, %{info | conn: conn, buffer: [rest]}}
    else
      :no_change
    end
  end
  
  defp handle_state(:recv_info_hash, %{conn: conn, info_hash: hash, buffer: buf} = info) do
    hashlen = byte_size(hash)
    if iolist_size(buf) >= hashlen do
      <<info_hash_enc::bytes-size(hashlen), rest::binary>> = iolist_to_binary(buf)
      {conn, info_hash} = Peer.Socket.decrypt(conn, info_hash_enc)
      if info_hash == hash do
        {:next_state, %{info | conn: conn, buffer: [rest]}}
      else
        {:error, :bad_info_hash}
      end
    else
      :no_change
    end
  end
  
  defp handle_state(:recv_peer_id, %{conn: conn, buffer: buf} = info) do
    if iolist_size(buf) >= 20 do
      <<peer_id_enc::bytes-size(20), rest::binary>> = iolist_to_binary(buf)
      {conn, peer_id} = Peer.Socket.decrypt(conn, peer_id_enc)
      {:next_state, %{info | conn: conn, peer_id: peer_id, buffer: [rest]}}
    else
      :no_change
    end
  end

  defp handle_state(state_name, _state) do
    raise "Unhandled state #{state_name}"
  end
end
