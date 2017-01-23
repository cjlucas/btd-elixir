defmodule Peer.Connection do
  use GenServer
  require Logger
  alias Bittorrent.Message.{Bitfield, Choke, Unchoke, Interested, NotInterested}

  defmodule State do
    defstruct info_hash: <<>>,
      sock: nil,
      bitfield: nil,
      choked: true,
      interested: false,
      in_bytes: 0,
      out_bytes: 0,
      buffer: <<>>
  end

  def start_link(info_hash, sock) do
    GenServer.start_link(__MODULE__, {info_hash, sock})
  end

  def send_msg(pid, msg) do
    GenServer.cast(pid, {:send_msg, msg})
  end

  def has_piece?(pid, idx) do
    GenServer.call(pid, {:has_piece?, idx})
  end

  def init({info_hash, sock}) do
    {:ok, {host, port}} = :inet.peername(sock.sock)
    host = host |> Tuple.to_list |> Enum.join(".")
    Peer.Registry.register(info_hash, {host, port})
    Peer.EventManager.received_connection(info_hash, self())
    {:ok, %State{info_hash: info_hash, sock: sock}}
  end

  def handle_info({:tcp, _sock, data}, %{info_hash: hash, sock: sock, buffer: buf} = state) do
    {sock, data} = Peer.Socket.decrypt(sock, data)
    buf = buf <> data

    case process_buffer(buf) do
      {:ok, msg, rest} ->
        Peer.EventManager.received_message(hash, {self(), msg})
        handle_msg(msg, %{state | sock: sock, buffer: rest})
      {:error, _} ->
        {:noreply, %{state | sock: sock, buffer: buf}}
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
