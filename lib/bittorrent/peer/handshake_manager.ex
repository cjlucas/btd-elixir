defmodule Peer.HandshakeManager do
  use GenServer
  require Logger

  @name __MODULE__
  
  @p 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563
  @vc <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>

  @pstr "BitTorrent protocol"

  # TODO: this should be in external config
  @sock_timeout 1000

  @outgoing_flow [
    :send_pubkey,
    :recv_pubkey,
    :calc_secret,
    :send_reqs,
    :init_stream,
    :send_vc,
    :send_crypto_provide,
    :send_pad,
    :send_ia,
  ]

  @incoming_flow [
    :recv_pubkey,
    :send_pubkey,
    :calc_secret,
    :recv_reqs
  ]

  defmodule Connection do
    defstruct in_stream: nil, out_stream: nil, sock: nil

    defp send(%Connection{out_stream: stream, sock: sock} = conn, data) when is_nil(stream) do
      case :gen_tcp.send(sock, data) do
        :ok -> {:ok, conn}
        {:error, reason} -> {:error, reason}
      end
    end

    defp send(%Connection{out_stream: stream, sock: sock} = conn, data) do
      {stream, data} = :crypto.stream_encrypt(stream, data)
      case :gen_tcp.send(sock, data) do
        :ok -> {:ok, %{conn | stream: stream}}
        {:error, reason} -> {:error, reason}
      end
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
              crypto_provide: 0
  end

  defmodule State do
    defstruct peers: %{}, buffers: %{}
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

  def register_peer(:out, conn) do
    with :ok <- GenServer.call(@name, {:register_peer, :out, conn}),
         :ok <- :gen_tcp.controlling_process(conn, Process.whereis(@name))
    do
      :ok
    end
  end

  defp trigger_handler(conn), do: GenServer.cast(@name, {:trigger_handler, conn})

  def handle_call({:register_peer, direction, conn}, _from, %State{peers: peers, buffers: buffers} = state) do
    flow = if direction == :in, do: @incoming_flow, else: @outgoing_flow
    peers = Map.put(peers, conn, %HandshakeInfo{incoming: direction == :in, states: flow})
    buffers = Map.put(buffers, conn, [])

    trigger_handler(conn)

    {:reply, :ok, %{state | peers: peers, buffers: buffers}}
  end

  def handle_cast({:trigger_handler, sock}, state), do: dispatch_handler(state.peers[sock], state)

  def handle_info({:tcp, sock, data}, %State{peers: peers} = state) do
    peer = peers[sock]

    unless is_nil(peer.conn.in_stream) do
      {stream, data} = :crypto.stream_decrypt(peer.conn.in_stream, data)
      peer = put_in(peer.conn.in_stream, stream)
    end

    GenServer.cast(@name, {:trigger_handler, sock})
    peers = update_in(peers[sock].buffer, &([&1, data]))
    IO.puts("OMGHERE #{inspect peers}")
    {:noreply, %{state | peers: peers}}
  end

  # state handlers
  
  defp dispatch_handler(info, state) when is_nil(info), do: {:noreply, state}
  defp dispatch_handler(%{conn: conn, states: [cur_state | rem_states]} = info, state) do
    case handle_state(cur_state, info, conn) do
      {:next_state, info} ->
        info = %{info | states: rem_states}
        IO.puts("Got :next_state, states now: #{inspect info.states}")
        trigger_handler(conn) # queue the next state handler
        {:noreply, put_in(state.peers[conn], info)}
      :no_change ->
        {:noreply, state}
      {:error, reason} ->
        # TODO: proper error handling
      IO.puts("Handler for state #{state} returned an error with reason #{reason}")
    end
  end
  
  defp handle_state(:send_pubkey, info, conn) do
    privkey = :crypto.strong_rand_bytes(rand(160))
    pubkey = :crypto.mod_pow(2, privkey, @p)
    pad = :crypto.strong_rand_bytes(rand(512))

    case Connection.send(conn, [pubkey, pad]) do
      {:ok, conn} ->
        {:next_state, %{info | privkey: privkey}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_pubkey, %HandshakeInfo{buffer: buf} = info, _conn) do
    IO.puts("GOT DATA IN THIS SHIT #{inspect buf}")
    if :erlang.iolist_size(buf) >= 96 do
      << pubkey :: bytes-size(96), rest :: binary >> = :erlang.iolist_to_binary(buf)
      IO.puts("set pubkey in info #{inspect pubkey}")
      {:next_state, %{info | pubkey: pubkey, buffer: [rest]}}
    else
      :no_change
    end
  end
  
  defp handle_state(:calc_secret, %HandshakeInfo{pubkey: pub, privkey: priv} = info, _conn) do
    {:next_state, %{info | secret: :crypto.mod_pow(pub, priv, @p)}}
  end

  defp handle_state(:send_reqs, %HandshakeInfo{secret: s, info_hash: info_hash} = info, conn) do
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

  defp handle_state(:recv_reqs, %HandshakeInfo{secret: s, buffer: buf} = info, _conn) do
    if :erlang.iolist_size(buf) >= 40 do
      <<
        req1 :: bytes-size(20),
        req23 :: bytes-size(20),
        rest :: binary
      >> = :erlang.iolist_to_binary(buf)

      # TODO: extract lookup req2 in torrent store

      case <<"req1">> <> s do
        ^req1 ->
          {:next_state, %{info | buffer: [rest]}}
        _ ->
          {:error, :bad_req1}
      end
    else
      :no_change
    end
  end

  defp handle_state(:init_streams, %HandshakeInfo{incoming: true, secret: s, info_hash: hash} = info, conn) do
    ins = :crypto.stream_init(:rc4, <<"keya">> <> s <> hash)
    outs = :crypto.stream_init(:rc4, <<"keyb">> <> s <> hash)
    {ins, _} = :crypto.stream_decrypt(ins, :crypto.strong_rand_bytes(1024))
    {outs, _} = :crypto.stream_encrypt(outs, :crypto.strong_rand_bytes(1024))

    {:next_state, %{info | conn: %{conn | in_stream: ins, out_stream: outs}}}
  end
  
  defp handle_state(:init_streams, %HandshakeInfo{incoming: false, secret: s, info_hash: hash} = info, conn) do
    ins = :crypto.stream_init(:rc4, <<"keyb">> <> s <> hash)
    outs = :crypto.stream_init(:rc4, <<"keya">> <> s <> hash)
    {ins, _} = :crypto.stream_decrypt(ins, :crypto.strong_rand_bytes(1024))
    {outs, _} = :crypto.stream_encrypt(outs, :crypto.strong_rand_bytes(1024))

    {:next_state, %{info | conn: %{conn | in_stream: ins, out_stream: outs}}}
  end

  defp handle_state(state, _info, _conn) do
    raise "Unhandled state #{state}"
  end

  defp key(prefix, s, skey), do: [prefix, s, skey]

  defp rand(n), do: :rand.uniform(n+1) - 1
  
  defp hash(buf), do: :crypto.hash(:sha, buf)

  defp bin_xor(bin1, bin2) do
    use Bitwise

    [l1, l2] = Enum.map([bin1, bin2], &:erlang.binary_to_list/1)
    Enum.zip(l1, l2)
    |> Enum.map(fn e -> elem(e, 0) ^^^ elem(e, 1) end)
    |> :erlang.list_to_binary
  end
end
