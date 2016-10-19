defmodule Bittorrent.Server do
  use GenServer

  @docmodule """
  TCP server for incoming peer connections
  """

  @name __MODULE__

  defmodule State do
    defstruct accept_loop_pid: nil, num_conns: 0
  end

  def start_link(port) do
    IO.puts("omghere")
    GenServer.start_link(@name, port, name: @name)
  end

  def num_conns do
    GenServer.call(Bittorrent.Server, :num_conns)
  end

  def init(port) do
    with {:ok, sock} <- :gen_tcp.listen(port, [:binary, active: false, reuseaddr: true]),
         pid <- spawn_link(fn -> accept(sock) end),
      do: {:ok, %State{accept_loop_pid: pid}}
  end

  def accept(sock) do
    {:ok, client} = :gen_tcp.accept(sock)
    GenServer.cast(Bittorrent.Server, {:received_connection, client})
    accept(sock)
  end

  def handle_call(:num_conns, _from, state) do
    {:reply, state.num_conns, state}
  end

  def handle_cast({:received_connection, sock}, state) do
    IO.puts("Received connection from somebody #{inspect sock}")
    # TODO: should handleshake handling be done here?
    #
    {:ok, pid} = Bittorrent.Connection.start_link(sock)
    IO.puts("Started connection server #{inspect pid}")
    :gen_tcp.controlling_process(sock, pid)
    {:noreply, %{state | num_conns: state.num_conns + 1}}
  end
end
