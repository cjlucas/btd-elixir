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

  def sync(_needle, <<>>), do: <<>>
  def sync(needle, haystack) do
    size = byte_size(needle)
    case haystack do
      << ^needle::bytes-size(size), rest::binary >> ->
        rest
      << _::8, rest::binary >> ->
        sync(needle, rest)
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

  defmodule HandshakeInfo do
    defstruct incoming: false, # NOTE: currently unused outside of determining state flow, may not need to be stored in struct
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

  defmodule State do
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
    defstruct peers: %{}, buffers: %{}

    def register_peer(%State{peers: peers, buffers: buffers} = state, direction, sock, info_hash \\ <<>>) do
      flow = if direction == :in, do: @incoming_flow, else: @outgoing_flow
      peers = Map.put(peers, sock, %HandshakeInfo{
                      incoming: direction == :in,
                      info_hash: info_hash,
                      conn: %Connection{sock: sock},
                      states: flow,
                    })
      buffers = Map.put(buffers, sock, [])

      %{state | peers: peers, buffers: buffers}
    end

    def unregister_peer(%State{peers: peers, buffers: buffers} = state, sock) do
      %{state | peers: Map.delete(peers, sock), buffers: Map.delete(buffers, sock)}
    end
  end

  def start_link do
    GenServer.start_link(@name, :ok, name: @name)
  end

  def init(:ok) do
    {:ok, %State{}}
  end

  #def register_peer(direction, sock) do
    #GenServer.call(@name, {:register_peer, :in, sock})
  #end

  def register_outgoing_peer(sock, info_hash) do
    with :ok <- GenServer.call(@name, {:register_outgoing_peer, sock, info_hash}),
         :ok <- :gen_tcp.controlling_process(sock, Process.whereis(@name))
    do
      :ok
    end
  end

  def peer_info(sock) do
    GenServer.call(@name, {:peer_info, sock})
  end

  defp trigger_handler(sock), do: GenServer.cast(@name, {:trigger_handler, sock})

  def handle_call({:register_outgoing_peer, sock, info_hash}, _from, state) do
    trigger_handler(sock)
    {:reply, :ok, State.register_peer(state, :out, sock, info_hash)}
  end
  
  def handle_call({:register_incoming_peer, sock}, _from, state) do
    trigger_handler(sock)
    {:reply, :ok, State.register_peer(state, :in, sock)}
  end

  def handle_call({:peer_info, sock}, _from, state) do
    IO.puts(inspect state.peers[sock])
    {:reply, state.peers[sock], state}
  end

  def handle_cast({:trigger_handler, sock}, state), do: dispatch_handler(state.peers[sock], state)

  def handle_info({:tcp, sock, data}, %State{peers: peers} = state) do
    trigger_handler(sock)
    Logger.info("GOT SOME DATA info = #{inspect peers[sock]}")
    peers = update_in(peers[sock].buffer, &([&1, data]))
    {:noreply, %{state | peers: peers}}
  end

  def handle_info({:tcp_closed, sock}, state) do
    {:noreply, State.unregister_peer(state, sock)}
  end

  # state handlers
  
  defp dispatch_handler(info, state) when is_nil(info), do: {:noreply, state}
  defp dispatch_handler(%{conn: conn, states: [cur_state | rem_states]} = info, state) do
    Logger.debug("cur_state = #{cur_state}")
    case handle_state(cur_state, info, conn) do
      {:next_state, info} ->
        Logger.debug("Got :next_state, states now: #{inspect rem_states}")
        info = %{info | states: rem_states}
        trigger_handler(conn.sock) # queue the next state handler
        {:noreply, put_in(state.peers[conn.sock], info)}
      :no_change ->
        Logger.debug("no_change state = #{inspect state}")
        {:noreply, state}
      {:error, reason} ->
        # TODO: proper error handling
        IO.puts("Handler for state returned an error with reason #{reason}")
        {:noreply, state}
    end
  end
  
  defp handle_state(:send_pubkey, info, conn) do
    {privkey, pubkey} = gen_keys()
    pad = :crypto.strong_rand_bytes(rand(512))

    Logger.debug("omgwtf pubkey = #{inspect pubkey}")

    case Connection.send(conn, [pubkey, pad]) do
      {:ok, conn} ->
        {:next_state, %{info | privkey: privkey, conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_pubkey, %{buffer: buf} = info, _conn) do
    Logger.debug("recv_pubkey, byte_size(buf) = #{iolist_size(buf)}")
    if iolist_size(buf) >= 96 do
      << pubkey :: bytes-size(96), rest :: binary >> = iolist_to_binary(buf)
      Logger.debug("got pubkey #{inspect pubkey}")
      {:next_state, %{info | pubkey: pubkey, buffer: [rest]}}
    else
      Logger.debug("DUDE HERE")
      :no_change
    end
  end
  
  defp handle_state(:calc_secret, %{pubkey: pub, privkey: priv} = info, _conn) do
    Logger.debug("S = #{inspect calc_secret(pub, priv)}")
    {:next_state, %{info | secret: calc_secret(pub, priv)}}
  end

  defp handle_state(:send_reqs, %{secret: s, info_hash: info_hash} = info, conn) do
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

  defp handle_state(:recv_reqs, %{secret: s, buffer: buf} = info, _conn) do
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

  defp handle_state(:init_streams, %{incoming: true, secret: s, info_hash: hash} = info, conn) do
    {ins, outs} = init_streams(key(<<"keyA">>, s, hash), key(<<"keyB">>, s, hash))
    {:next_state, put_in(info.conn,  %{conn | in_stream: ins, out_stream: outs})}
  end
  
  defp handle_state(:init_streams, %{incoming: false, secret: s, info_hash: hash} = info, conn) do
    {ins, outs} = init_streams(key(<<"keyB">>, s, hash), key(<<"keyA">>, s, hash))
    {:next_state, put_in(info.conn,  %{conn | in_stream: ins, out_stream: outs})}
  end

  defp handle_state(:send_vc, info, conn) do
    case Connection.send(conn, @vc) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_vc, %{expected_vc: vc} = info, conn) when is_nil(vc) do
    {stream, vc} = :crypto.stream_encrypt(conn.in_stream, @vc)
    conn = %{conn | in_stream: stream}
    handle_state(:recv_vc, %{info | expected_vc: vc, conn: conn}, conn)
  end
  
  defp handle_state(:recv_vc, %{expected_vc: vc, buffer: buf} = info, _conn) do
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

  defp handle_state(:recv_crypto_select, %{buffer: buf} = info, conn) do
    if iolist_size(buf) >= byte_size(@crypto_provide) do
      << v::32, rest::binary >> = iolist_to_binary(buf)
      {:next_state, %{info | crypto_select: v, buffer: [rest]}}
    else
      :no_change
    end
  end
  
  defp handle_state(:send_crypto_provide, info, conn) do
    case Connection.send(conn, @crypto_provide) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_pad, %{buffer: buf}, _conn) when byte_size(buf) < 2 do
    :no_change
  end
  
  defp handle_state(:recv_pad, %{buffer: buf} = info, _conn) do
    << len::16, rest::binary >> = buf
    case rest do
      << _::bytes-size(len), rest::binary>> ->
        {:next_state, %{info | buffer: [rest]}}
      _ ->
        :no_change
    end
  end

  defp handle_state(:send_pad, info, conn) do
    # only send len(padX)
    case Connection.send(conn, <<0x00, 0x00>>) do
      {:ok, conn} ->
        {:next_state, %{info | conn: conn}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:send_ia, info, conn) do
    ialen = 49 + @pstrlen
    case Connection.send(conn, <<ialen::16>>) do
      {:ok, conn} ->
        handle_state(:send_handshake, info, conn)
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:send_handshake, %{info_hash: info_hash} = info, conn) do
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

  defp handle_state(state, _info, _conn) do
    raise "Unhandled state #{state}"
  end
end
