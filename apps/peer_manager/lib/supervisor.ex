defmodule Peer.Supervisor do
  use Supervisor

  @name __MODULE__

  def start_link do
    Supervisor.start_link(@name, :ok, name: @name)
  end

  def connect_peer({addr, port}, opts) do
    Task.Supervisor.start_child(Peer.ConnectTask.Supervisor, fn ->
      case :gen_tcp.connect(addr, port, [:binary, active: true]) do
        {:ok, sock} ->
          {:ok, pid} = supervise_connection(sock)
          :gen_tcp.controlling_process(sock, pid)
        {:error, reason} ->
          {:error, reason}
      end
    end)
  end

  def supervise_connection(sock) do
    Supervisor.start_child(@name, worker(Peer.Connection, [{:in, sock}], restart: :transient))
  end

  def init(:ok) do
    children = [
      worker(Peer.Server, [60000]),
      supervisor(Task.Supervisor, [[name: Peer.ConnectTask.Supervisor]]),
      worker(Peer.Manager, []),
      worker(Peer.HandshakeManager, [])
    ]

    supervise(children, strategy: :one_for_one)
  end

end
