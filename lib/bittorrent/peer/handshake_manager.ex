defmodule Peer.Handshake do
  @p 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563

  def gen_keys do
    privkey = :crypto.strong_rand_bytes(rand(160))
    pubkey = :crypto.mod_pow(2, privkey, @p)
    {privkey, pubkey}
  end

  def calc_secret(pubkey, privkey), do: :crypto.mod_pow(pubkey, privkey, @p)

  def init_streams(in_key, out_key) do
    [ins, outs] = [in_key, out_key]
                  |> Enum.map(&(:crypto.stream_init(:rc4, &1)))
                  |> Enum.map(&(:crypto.stream_decrypt(&1, :crypto.strong_rand_bytes(1024))))
                  |> Enum.map(&(elem(&1, 0)))
    {ins, outs}
  end

  def req1(s), do: hash([<<"req1">>, s])

  def req2(skey), do: hash([<<"req2">>, skey])
  
  def req3(s), do: hash([<<"req3">>, s])

  def key(prefix, s, skey), do: hash([prefix, s, skey])
  
  def hash(buf), do: :crypto.hash(:sha, buf)
  
  def rand(n), do: :rand.uniform(n+1) - 1

  def sync(needle, haystack), do: sync(needle, haystack, 0)
  defp sync(needle, haystack, _offset) when byte_size(needle) > byte_size(haystack), do: haystack
  defp sync(_needle, haystack, offset) when offset > byte_size(haystack), do: haystack
  defp sync(needle, haystack, offset) do
    size = byte_size(needle)
    case haystack do
      << _::bytes-size(offset), ^needle::bytes-size(size), rest::binary >> ->
        rest
      _ ->
        sync(needle, haystack, offset+1)
    end
  end

  def bin_xor(bin1, bin2) do
    use Bitwise

    [l1, l2] = Enum.map([bin1, bin2], &:erlang.binary_to_list/1)
    Enum.zip(l1, l2)
    |> Enum.map(fn e -> elem(e, 0) ^^^ elem(e, 1) end)
    |> :erlang.list_to_binary
  end
end

defmodule Peer.HandshakeManager do
  import Peer.Handshake
  import :erlang, only: [iolist_size: 1, iolist_to_binary: 1]
  use GenServer
  require Logger

  @name __MODULE__
  
  @vc <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>
  @crypto_provide <<2::32>> # rc4 only

  @pstr "BitTorrent protocol"
  @pstrlen 19

  # TODO: this should be in external config
  @sock_timeout 1000

  defmodule Connection do
    defstruct in_stream: nil, out_stream: nil, sock: nil

    def send(%Connection{out_stream: stream, sock: sock} = conn, data) when is_nil(stream) do
      case :gen_tcp.send(sock, data) do
        :ok -> {:ok, conn}
        {:error, reason} -> {:error, reason}
      end
    end

    def send(%Connection{out_stream: stream, sock: sock} = conn, data) do
      {stream, data} = :crypto.stream_encrypt(stream, data)
      case :gen_tcp.send(sock, data) do
        :ok -> {:ok, %{conn | out_stream: stream}}
        {:error, reason} -> {:error, reason}
      end
    end

    def recv(%Connection{sock: sock} = conn, len, timeout \\ 1000) do
      with {:ok, data} <- :gen_tcp.recv(sock, len, timeout) do
        {:ok, decrypt(conn, data)}
      end
    end

    def decrypt(%Connection{in_stream: stream} = conn, data) do
      {stream, data} = :crypto.stream_decrypt(stream, data)
      {%{conn | in_stream: stream}, data}
    end
  end

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
    :recv_reqs
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

  def handle_info(:timeout, %InitialState{lsock: sock}) when not is_nil(sock) do
    {:ok, sock} = :gen_tcp.accept(sock)
    :inet.setopts(sock, [active: true])
    conn = %Connection{sock: sock}
    {:noreply, %State{incoming: true, states: @incoming_flow, conn: conn}}
  end
  
  def handle_info(:timeout, %InitialState{host: host, port: port, info_hash: hash}) do
    {:ok, sock} = :gen_tcp.connect(host, port, [:binary, active: true])
    conn = %Connection{sock: sock}
    trigger_handler()
    {:noreply, %State{incoming: false, states: @outgoing_flow, info_hash: hash, conn: conn}}
  end

  def handle_info({:tcp, _sock, data}, state) do
    trigger_handler()
    {:noreply, update_in(state.buffer, &([&1, data]))}
  end

  def handle_info({:tcp_closed, _sock}, _state) do
    {:stop, :closed}
  end

  # state handlers
  
  defp dispatch_handler(%{states: []} = state) do
    {:stop, :normal, state}
  end
  defp dispatch_handler(%{states: [cur_state | rem_states]} = state) do
    case handle_state(cur_state, state) do
      {:next_state, info} ->
        trigger_handler() # queue the next state handler
        {:noreply, %{info | states: rem_states}}
      :no_change ->
        {:noreply, state}
      {:error, _} ->
        # TODO: proper error handling
        {:noreply, state}
    end
  end
  
  defp handle_state(:send_pubkey, %{conn: conn} = info) do
    {privkey, pubkey} = gen_keys()
    pad = :crypto.strong_rand_bytes(rand(512))

    case Connection.send(conn, [pubkey, pad]) do
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
    {:next_state, %{info | secret: calc_secret(pub, priv)}}
  end

  defp handle_state(:send_reqs, %{conn: conn, secret: s, info_hash: info_hash} = info) do
    payload = [
      hash([<<"req1">>, s]),
      bin_xor(hash([<<"req2">>, info_hash]), hash([<<"req3">>, s]))
    ]

    case Connection.send(conn, payload) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_reqs, %{secret: s, buffer: buf} = info) do
    if iolist_size(buf) >= 40 do
      <<
        req1buf :: bytes-size(20),
        req23buf :: bytes-size(20),
        rest :: binary
      >> = iolist_to_binary(buf)

      # TODO: extract lookup req2 in torrent store

      case req1(s) do
        ^req1buf ->
          {:next_state, %{info | buffer: [rest]}}
        _ ->
          {:error, :bad_req1}
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
    case Connection.send(conn, @vc) do
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
      rest = sync(vc, iolist_to_binary(buf))
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
      << vc_enc::bytes-size(4), rest::binary>> = iolist_to_binary(buf)
      {conn, vc} = Connection.decrypt(conn, vc_enc)
      {:next_state, %{info | conn: conn, crypto_select: vc, buffer: [rest]}}
    else
      :no_change
    end
  end
  
  defp handle_state(:send_crypto_provide, %{conn: conn} = info) do
    case Connection.send(conn, @crypto_provide) do
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
    << enc_len::bytes-size(2), rest::binary >> = iolist_to_binary(buf)
    {conn, <<len::16>>} = Connection.decrypt(conn, enc_len)
    case rest do
      << pad::bytes-size(len), rest::binary>> ->
        {conn, _} = Connection.decrypt(conn, pad)
        {:next_state, %{info | conn: conn, buffer: [rest]}}
      _ ->
        :no_change
    end
  end

  defp handle_state(:send_pad, %{conn: conn} = info) do
    # only send len(padX)
    case Connection.send(conn, <<0x00, 0x00>>) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:send_ia, %{conn: conn} = info) do
    ialen = 49 + @pstrlen
    case Connection.send(conn, <<ialen::16>>) do
      {:ok, conn} ->
        handle_state(:send_handshake, %{info | conn: conn})
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:send_handshake, %{conn: conn, info_hash: info_hash} = info) do
    case Torrent.Store.lookup(:info_hash, info_hash) do
      {:ok, t} ->
        payload = [
          <<@pstrlen::8>>,
          @pstr,
          <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>,
          info_hash,
          t.peer_id,
        ]
        case Connection.send(conn, payload) do
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
      {conn, <<pstrlen::8>>} = Connection.decrypt(conn, pstrlen_enc)
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
      {conn, pstr} = Connection.decrypt(conn, pstr_enc)
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
      {conn, reserved} = Connection.decrypt(conn, reserved_enc)
      if reserved == <<0::64>> do
        {:next_state, %{info | conn: conn, buffer: [rest]}}
      else
        {:error, :bad_reserved}
      end
    else
      :no_change
    end
  end
  
  defp handle_state(:recv_info_hash, %{conn: conn, info_hash: hash, buffer: buf} = info) do
    hashlen = byte_size(hash)
    if iolist_size(buf) >= hashlen do
      <<info_hash_enc::bytes-size(hashlen), rest::binary>> = iolist_to_binary(buf)
      {conn, info_hash} = Connection.decrypt(conn, info_hash_enc)
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
      {conn, peer_id} = Connection.decrypt(conn, peer_id_enc)
      {:next_state, %{info | conn: conn, peer_id: peer_id, buffer: [rest]}}
    else
      :no_change
    end
  end

  defp handle_state(state_name, _state) do
    raise "Unhandled state #{state_name}"
  end
end
