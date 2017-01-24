defmodule Peer.Connection.ReadHandler do
  def loop(s, pid) do
    receive do
      read_amnt when is_integer(read_amnt) ->
        case :gen_tcp.recv(s, read_amnt) do
          {:ok, data} ->
            send(pid, {:read_data, data})
          {:error, reason} ->
            send(pid, {:read_error, reason})
        end

        loop(s, pid)
      _ ->
        raise "Received unexpected message"
    end
  end
end

defmodule Peer.Connection do
  use GenServer
  require Logger
  alias Bittorrent.Message.{Bitfield, Choke, Unchoke, Interested, NotInterested}

  defmodule State do

    @type read_state :: :awaiting_length | :awaiting_payload

    defstruct info_hash: <<>>,
      sock: nil,
      initial_data: nil,
      bitfield: nil,
      choked: true,
      interested: false,
      in_bytes: 0,
      out_bytes: 0,
      read_pid: nil,
      read_state: :awaiting_length
  end

  def start_link(info_hash, sock, initial_data) do
    GenServer.start_link(__MODULE__, {info_hash, sock, initial_data})
  end

  def send_msg(pid, msg) do
    GenServer.cast(pid, {:send_msg, msg})
  end

  def has_piece?(pid, idx) do
    GenServer.call(pid, {:has_piece?, idx})
  end

  def init({info_hash, %{sock: s} = sock, initial_data}) do
    {:ok, {host, port}} = :inet.peername(sock.sock)
    host = host |> Tuple.to_list |> Enum.join(".")
    Peer.Registry.register(info_hash, {host, port})
    Peer.EventManager.received_connection(info_hash, self())

    pid = self()
    read_pid = spawn_link(fn ->
      Peer.Connection.ReadHandler.loop(s,pid)
    end)

    {sock, initial_data} = Peer.Socket.decrypt(sock, initial_data)

    data = case process_buffer(initial_data) do
      {:ok, msg, rest} ->
        IO.puts("GOT MSG #{inspect msg}")
        Peer.EventManager.received_message(info_hash, {self(), msg})
        rest
      {:error, reason} ->
        IO.puts("DIDNT GET MSG")
        initial_data
    end

    {sock, read_state, data} =
      if byte_size(data) >= 4 do
        <<len::32, data::binary>> = data
        IO.puts("WTFHERE #{len} #{byte_size(data)}")
        send(read_pid, len - byte_size(data))
        {sock, :awaiting_payload, data}
      else
        IO.puts("OMGHERE #{byte_size(data)}")
        send(read_pid, 4)
        {sock, :awaiting_length, nil}
      end


    {:ok, %State{info_hash: info_hash, sock: sock, initial_data: data, read_pid: read_pid}}
  end

  def handle_info({:read_data, data}, %{initial_data: buf} = state) when is_binary(buf) do
    handle_info({:read_data, buf <> data}, %{state | initial_data: nil})
  end

  def handle_info({:read_data, data}, %{read_state: st, sock: sock, read_pid: pid} = state)
      when st == :awaiting_length do
    {sock, <<len::32>>} = Peer.Socket.decrypt(sock, data)
    send(pid, len)

    #IO.puts("GOT LEN #{len}")

    {:noreply, %{state | read_state: :awaiting_payload, sock: sock}}
  end

  def handle_info({:read_data, data}, %{read_state: st, info_hash: hash, sock: sock, read_pid: pid} = state)
      when st == :awaiting_payload do
    {sock, data} = Peer.Socket.decrypt(sock, data)

    #IO.puts("GOT DATA #{inspect data}")

    send(pid, 4)

    case Bittorrent.Message.parse(data) do
      {:ok, msg} ->
        #IO.puts("GOT MSG #{inspect msg}")
        Peer.EventManager.received_message(hash, {self(), msg})
        handle_msg(msg, %{state | read_state: :awaiting_length, sock: sock})
      {:error, _} ->
        {:noreply, %{state | read_state: :awaiting_length, sock: sock}}
    end
  end

  def handle_info({:tcp_closed, _sock}, state) do
    {:stop, :normal, state}
  end

  def handle_call({:has_piece?, _idx}, _from, %{bitfield: bits} = state) when is_nil(bits) do
    {:reply, false, state}
  end
  def handle_call({:has_piece?, idx}, _from, %{bitfield: bits} = state) do
    {:reply, BitSet.get(bits, idx) == 1, state}
  end

  def handle_cast({:send_msg, msg}, %{info_hash: hash, sock: sock} = state) do
    data = Bittorrent.Message.encode(msg)
    case Peer.Socket.send(sock, [<<byte_size(data)::32>>, data]) do
      {:ok, sock} ->
        Peer.EventManager.sent_message(hash, {self(), msg})
        {:noreply, %{state | sock: sock}}
      {:error, reason} ->
        Logger.debug("Send failed with reason: #{reason}")
        {:stop, :normal, state}
    end
  end

  def terminate(_reason, %{sock: sock}) do
    Peer.Socket.close(sock)
  end

  defp process_buffer(<<0::32, rest::binary>>), do: process_buffer(rest)
  defp process_buffer(<<len::32, rest::binary>>) when byte_size(rest) >= len do
    <<payload::bytes-size(len), rest::binary>> = rest
    case Bittorrent.Message.parse(payload) do
      {:ok, msg} ->
        {:ok, msg, rest}
      {:error, reason} ->
        {:error, reason}
    end
  end
  defp process_buffer(_buf) do
    {:error, :insufficient_data}
  end

  defp handle_msg(%Bitfield{bitfield: bits}, state) do
    {:noreply, %{state | bitfield: BitSet.from_binary(bits)}}
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
end
