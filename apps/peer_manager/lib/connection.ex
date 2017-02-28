defmodule Peer.Connection.ReadHandler do
  def loop(s, pid, initial_data) do
    receive do
      read_amnt when is_integer(read_amnt) ->
        case read(read_amnt, s, initial_data) do
          {:ok, data, rem_data} ->
            send(pid, {:read_data, data})
            loop(s, pid, rem_data)
          {:error, reason} ->
            send(pid, {:read_error, reason})
            loop(s, pid, initial_data)
        end
      _ ->
        raise "Received unexpected message"
    end
  end

  def read(n, s, initial_data) when byte_size(initial_data) == 0 do
    case :gen_tcp.recv(s, n) do
      {:ok, data}      -> {:ok, data, <<>>}
      {:error, reason} -> {:error, reason}
    end
  end
  def read(n, _s, initial_data) when byte_size(initial_data) >= n do
    <<data::bytes-size(n), rest::binary>> = initial_data
    {:ok, data, rest}
  end
  def read(n, s, initial_data) when byte_size(initial_data) < n do
    with {:ok, buf_data, _}  <- read(byte_size(initial_data), s, initial_data),
         {:ok, sock_data, _} <- read(n - byte_size(initial_data), s, <<>>)
    do
      {:ok, buf_data <> sock_data, <<>>}
    end
  end
end

defmodule Peer.Connection do
  use GenServer
  require Logger
  alias Bittorrent.Message.{Bitfield, Have, Choke, Unchoke, Interested, NotInterested}

  defmodule State do

    @type read_state :: :awaiting_length | :awaiting_payload

    defstruct info_hash: <<>>,
      peer_id: <<>>,
      sock: nil,
      bitfield: nil,
      choked: true,
      interested: false,
      in_bytes: 0,
      out_bytes: 0,
      read_pid: nil,
      read_state: :awaiting_length
  end

  def start_link(info_hash, peer_id, sock, initial_data) do
    name = via(info_hash, peer_id)
    GenServer.start_link(__MODULE__, {info_hash, peer_id, sock, initial_data}, name: name)
  end

  def send_msg(info_hash, peer_id, msg) do
    via(info_hash, peer_id) |> GenServer.cast({:send_msg, msg})
  end

  def has_piece?(info_hash, peer_id, idx) do
    via(info_hash, peer_id) |> GenServer.cast({:has_piece?, idx})
  end

  def init({info_hash, peer_id, %{sock: s} = sock, initial_data}) do
    #{:ok, {host, port}} = :inet.peername(sock.sock)
    #host = host |> Tuple.to_list |> Enum.join(".")
    :ok = Peer.Swarm.Registry.register(info_hash, peer_id)
    Peer.EventManager.peer_connected(info_hash, peer_id)

    pid = self()
    read_pid = spawn_link(fn ->
      Peer.Connection.ReadHandler.loop(s,pid, initial_data)
    end)

    send(read_pid, 4)

    {:ok, %State{info_hash: info_hash, peer_id: peer_id, sock: sock, read_pid: read_pid}}
  end

  def handle_info({:read_data, data}, %{read_state: st, sock: sock, read_pid: pid} = state)
      when st == :awaiting_length do
    {sock, <<len::32>>} = Peer.Socket.decrypt(sock, data)

    {read_state, len} = case len do
      0 -> {:awaiting_length, 4}
      _ -> {:awaiting_payload, len}
    end

    send(pid, len)

    {:noreply, %{state | read_state: read_state, sock: sock}}
  end

  def handle_info({:read_data, data}, %{read_state: st, info_hash: hash, peer_id: id, sock: sock, read_pid: pid} = state)
      when st == :awaiting_payload do
    {sock, data} = Peer.Socket.decrypt(sock, data)

    send(pid, 4)

    state = %{state | read_state: :awaiting_length, sock: sock}

    case Bittorrent.Message.parse(data) do
      {:ok, msg} ->
        Peer.EventManager.received_message(hash, {id, msg})
        handle_msg(msg, state)
      {:error, reason} ->
        Logger.debug("Error parsing packet: #{reason}")
        Logger.debug(byte_size data)
        Logger.debug(inspect data)
        {:noreply, state}
    end
  end

  def handle_info({:read_error, reason}, state) do
    Logger.debug("Got a read error (reason: #{reason})")
    {:stop, :normal, state}
  end

  def handle_call({:has_piece?, _idx}, _from, %{bitfield: bits} = state) when is_nil(bits) do
    {:reply, false, state}
  end
  def handle_call({:has_piece?, idx}, _from, %{bitfield: bits} = state) do
    {:reply, BitSet.get(bits, idx) == 1, state}
  end

  def handle_cast({:send_msg, msg}, %{info_hash: hash, peer_id: id, sock: sock} = state) do
    data = Bittorrent.Message.encode(msg)
    case Peer.Socket.send(sock, [<<byte_size(data)::32>>, data]) do
      {:ok, sock} ->
        Peer.EventManager.sent_message(hash, {id, msg})
        {:noreply, %{state | sock: sock}}
      {:error, reason} ->
        Logger.debug("Send failed with reason: #{reason}")
        {:stop, :normal, state}
    end
  end

  def terminate(_reason, %{info_hash: info_hash, peer_id: id, sock: sock}) do
    Logger.debug("In terminate")
    Peer.EventManager.peer_disconnected(info_hash, id)
    Peer.Socket.close(sock)
  end

  defp handle_msg(%Bitfield{bitfield: bits}, state) do
    %{info_hash: info_hash} = state

    bs = BitSet.from_binary(bits)
    piece_idxs =
      bs
      |> Enum.to_list
      |> Enum.with_index
      |> Enum.filter(&elem(&1, 0) == 1)
      |> Enum.map(&elem(&1, 1))

    :ok = Peer.PieceRarity.seen_pieces(info_hash, piece_idxs)

    {:noreply, %{state | bitfield: bs}}
  end

  defp handle_msg(%Have{index: idx}, state) do
    %{info_hash: info_hash, bitfield: bf} = state
    :ok = Peer.PieceRarity.seen_pieces(info_hash, idx)

    {:noreply, %{state | bitfield: BitSet.set(bf, idx, 1)}}
  end

  defp handle_msg(%Choke{}, state) do
    {:noreply, %{state | choked: true}}
  end

  defp handle_msg(%Unchoke{}, state) do
    {:noreply, %{state | choked: false}}
  end

  defp handle_msg(%Interested{}, state) do
    {:noreply, %{state | interested: true}}
  end

  defp handle_msg(%NotInterested{}, state) do
    {:noreply, %{state | interested: false}}
  end

  defp handle_msg(_msg, state) do
    {:noreply, state}
  end

  defp via(info_hash, peer_id) do
    key = {info_hash, peer_id}
    {:via, Registry, {Peer.Registry, key}}
  end
end
