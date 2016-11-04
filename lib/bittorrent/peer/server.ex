defmodule Peer.Server do
  use GenServer

  @docmodule """
  TCP server for incoming peer connections
  """

  @name __MODULE__

  defmodule State do
    defstruct accept_pid: nil
  end

  def start_link(port) do
    GenServer.start_link(@name, port, name: @name)
  end

  def init(port) do
    case :gen_tcp.listen(port, [:binary, active: true, reuseaddr: true, backlog: 65535]) do
      {:ok, sock} ->
        pid = self()
        pid = spawn_link(fn -> accept(pid, sock) end)
        Process.monitor(pid)
        {:ok, %State{accept_pid: pid}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  def handle_cast({:received_connection, sock}, state) do
    IO.puts("Received connection from somebody #{inspect sock}")
    {:ok, pid} = Peer.Supervisor.supervise_connection(sock)
    :ok = :gen_tcp.controlling_process(sock, pid)
    IO.puts("Started connection server #{inspect pid}")
    {:noreply, state}
  end
  
  defp accept(pid, sock) do
    case :gen_tcp.accept(sock) do
      {:ok, client} ->
        :ok = :gen_tcp.controlling_process(client, pid)
        GenServer.cast(@name, {:received_connection, client})
        accept(pid, sock)
      {:error, reason} ->
        GenServer.stop(@name, reason)
    end
  end
end
