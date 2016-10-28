defmodule Peer.Connection do
  use GenServer

  defmodule State do
    defstruct sock: nil, recv_pid: nil
  end

  def start_link(sock) do
    GenServer.start_link(__MODULE__, {:ok, sock})
  end

  def send_msg(pid, msg) do
    GenServer.cast(pid, {:send_msg, msg}) 
  end

  def init({:ok, sock}) do
    pid = self()

    with pid <- spawn_link(fn -> recv_loop(pid, sock) end),
      do: {:ok, %State{sock: sock, recv_pid: pid}}
  end

  def handle_cast({:send_msg, msg}, state = %State{sock: sock}) do
    case :gen_tcp.send(sock, Bittorrent.Message.encode(msg)) do
      :ok ->
        {:noreply, state}
      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  def handle_call({:received_message, msg}, _from, state) do
    IO.puts("Received message: #{inspect msg}")
    {:reply, :ok, state}
  end

  def recv_loop(pid, sock) do
    with {:ok, << len :: big-size(32) >>} <- :gen_tcp.recv(sock, 4),
         {:ok, msg} <- recv_msg(sock, len) do
           GenServer.call(pid, {:received_message, msg})
           :gen_tcp.send(sock, Bittorrent.Message.encode(msg))
           recv_loop(pid, sock)
         else
           {:error, reason} ->
            IO.puts("Will close connection (reason: #{reason})")
            :gen_tcp.close(sock)
            GenServer.stop(pid, :normal)
         end
  end

  def recv_msg(sock, len) do
    with {:ok, data} <- :gen_tcp.recv(sock, len),
      do: Bittorrent.Message.parse(data)
  end
end
