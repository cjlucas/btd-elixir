defmodule Peer.HandshakeManager do
  use GenServer
  require Logger

  @name __MODULE__
  
  @p 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563
  @vc <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>

  @pstr "BitTorrent protocol"


  # TODO: this should be in external config
  @sock_timeout 1000

  defmodule HandshakeInfo do
    defstruct incoming: false,
              state: nil,
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

  def handle_call({:register_peer, direction, sock}, _from, %State{peers: peers, buffers: buffers} = state) do
    peers = Map.put(peers, sock, %HandshakeInfo{incoming: direction == :in})
    buffers = Map.put(buffers, sock, [])
    {:reply, :ok, %{state | peers: peers, buffers: buffers}}
  end

  def handle_call({:received_data, sock}, _from, state) do
    case handle_state(state.peers[sock]) do
      {:ok, info, buffer} ->

    end
  end

  def handle_info({:tcp, sock, data}, state) do
    GenServer.call(@name, {:received_data, sock})
    {:noreply, put_in(state, [:buffers, sock], [state.buffers[sock], data])}
  end

  # state handlers

  defp handle_state(%HandshakeInfo{incoming: false, state: :recv_pubkey}, buffer) do
  end

  # state modifiers

  defp send_pubkey(state) do
    privkey = :crypto.strong_rand_bytes(rand(160))
    pubkey = :crypto.mod_pow(2, privkey, @p)
    pad = :crypto.strong_rand_bytes(rand(512))

    case :gen_tcp.send(state.sock, [pubkey, pad]) do
      :ok ->
        {:ok, %{state | privkey: privkey}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp rand(n), do: :rand.uniform(n+1) - 1
end
