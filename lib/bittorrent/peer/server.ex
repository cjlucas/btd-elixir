defmodule Peer.Server do
  use GenServer

  @docmodule """
  TCP server for incoming peer connections
  """

  @name __MODULE__

  defmodule State do
    defstruct accept_pid: nil, num_conns: 0
  end

  def start_link(port) do
    GenServer.start_link(@name, port, name: @name)
  end

  def init(port) do
    case :gen_tcp.listen(port, [:binary, active: false, reuseaddr: true, backlog: 65535]) do
      {:ok, sock} ->
        pid = spawn_link(fn -> accept(sock) end)
        Process.monitor(pid)
        {:ok, %State{accept_pid: pid}}
      {:error, reason} ->
        {:error, reason}
    end
  end

  def handle_cast({:received_connection, sock}, state) do
    IO.puts("Received connection from somebody #{inspect sock}")
    # TODO: should handleshake handling be done here?
    #
    {:ok, pid} = Peer.Connection.start_link(sock)
    IO.puts("Started connection server #{inspect pid}")
    :gen_tcp.controlling_process(sock, pid)
    {:noreply, %{state | num_conns: state.num_conns + 1}}
  end
  
  defp accept(sock) do
    case :gen_tcp.accept(sock) do
      {:ok, client} ->
        GenServer.cast(@name, {:received_connection, client})
        accept(sock)
      {:error, reason} ->
        GenServer.stop(@name, reason)
    end
  end
end
