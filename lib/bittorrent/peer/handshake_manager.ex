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
    :send_reqs
  ]

  @incoming_flow [
    :recv_pubkey,
    :send_pubkey,
    :calc_secret,
    :recv_reqs
  ]

  defmodule HandshakeInfo do
    defstruct incoming: false, # NOTE: currently unused outside of determining state flow, may not need to be stored in struct
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

  def register_peer(:out, sock) do
    with :ok <- GenServer.call(@name, {:register_peer, :out, sock}),
         :ok <- :gen_tcp.controlling_process(sock, Process.whereis(@name))
    do
      :ok
    end
  end

  defp trigger_handler(sock), do: GenServer.cast(@name, {:trigger_handler, sock})

  def handle_call({:register_peer, direction, sock}, _from, %State{peers: peers, buffers: buffers} = state) do
    flow = if direction == :in, do: @incoming_flow, else: @outgoing_flow
    peers = Map.put(peers, sock, %HandshakeInfo{incoming: direction == :in, states: flow})
    buffers = Map.put(buffers, sock, [])

    trigger_handler(sock)

    {:reply, :ok, %{state | peers: peers, buffers: buffers}}
  end

  def handle_cast({:trigger_handler, sock}, state), do: dispatch_handler(sock, state.peers[sock], state)

  def handle_info({:tcp, sock, data}, %State{peers: peers} = state) do
    GenServer.cast(@name, {:trigger_handler, sock})
    peers = update_in(peers[sock].buffer, &([&1, data]))
    IO.puts("OMGHERE #{inspect peers}")
    {:noreply, %{state | peers: peers}}
  end

  # state handlers
  
  defp dispatch_handler(_sock, info, state) when is_nil(info), do: {:noreply, state}
  defp dispatch_handler(sock, %{states: [cur_state | rem_states]} = info, state) do
    case handle_state(cur_state, info, sock) do
      {:next_state, info} ->
        info = %{info | states: rem_states}
        IO.puts("Got :next_state, states now: #{inspect info.states}")
        trigger_handler(sock) # queue the next state handler
        {:noreply, put_in(state.peers[sock], info)}
      :no_change ->
        {:noreply, state}
      {:error, reason} ->
        # TODO: proper error handling
      IO.puts("Handler for state #{state} returned an error with reason #{reason}")
    end
  end
  
  defp handle_state(:send_pubkey, info, sock) do
    privkey = :crypto.strong_rand_bytes(rand(160))
    pubkey = :crypto.mod_pow(2, privkey, @p)
    pad = :crypto.strong_rand_bytes(rand(512))

    case :gen_tcp.send(sock, [pubkey, pad]) do
      :ok ->
        {:next_state, %{info | privkey: privkey}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handle_state(:recv_pubkey, %HandshakeInfo{buffer: buf} = info, _sock) do
    IO.puts("GOT DATA IN THIS SHIT #{inspect buf}")
    if :erlang.iolist_size(buf) >= 96 do
      << pubkey :: bytes-size(96), rest :: binary >> = :erlang.iolist_to_binary(buf)
      IO.puts("set pubkey in info #{inspect pubkey}")
      {:next_state, %{info | pubkey: pubkey, buffer: [rest]}}
    else
      :no_change
    end
  end
  
  defp handle_state(:calc_secret, %HandshakeInfo{pubkey: pub, privkey: priv} = info, _sock) do
    {:next_state, %{info | secret: :crypto.mod_pow(pub, priv, @p)}}
  end

  defp handle_state(state, _info, _sock) do
    raise "Unhandled state #{state}"
  end

  defp rand(n), do: :rand.uniform(n+1) - 1
end
