defmodule Peer.Connection do
  use GenServer
  require Logger

  defmodule State do
    defstruct sock: nil,
      bitfield: <<>>,
      choked: true,
      interested: false,
      in_bytes: 0,
      out_bytes: 0,
      buffer: <<>>
  end

  def start_link(sock) do
    GenServer.start_link(__MODULE__, sock)
  end

  def send_msg(pid, msg) do
    GenServer.cast(pid, {:send_msg, msg})
  end

  def close(pid) do
    GenServer.call(pid, :close)
  end

  def init(sock) do
    {:ok, %State{sock: sock}} 
  end

  def handle_info({:tcp, _sock, data}, %{sock: sock, buffer: buf} = state) do
    {sock, data} = Peer.Socket.decrypt(sock, data)
    buf = buf <> data

    case process_buffer(buf) do
      {:ok, msg, rest} ->
        handle_msg(msg, %{state | sock: sock, buffer: rest})
      {:error, _} ->
        {:noreply, %{state | sock: sock, buffer: buf}}
    end
  end

  def handle_info({:tcp_closed, _sock}, state) do
    {:stop, :normal, state}
  end

  def handle_call(:close, _from, %{sock: sock} = state) do
    {:reply, :gen_tcp.close(sock.sock), state}
  end

  def handle_cast({:send_msg, msg}, %{sock: sock} = state) do
    case Peer.Socket.send(sock, msg) do
      {:ok, sock} ->
        {:noreply, %{state | sock: sock}}
      {:error, reason} ->
        Logger.debug("Send failed with reason: #{reason}")
        {:stop, :normal, state}
    end
  end

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

  defp handle_msg(msg, state) do
    Logger.debug("received unhandled msg #{inspect msg}") 
    {:noreply, state}
  end
end
