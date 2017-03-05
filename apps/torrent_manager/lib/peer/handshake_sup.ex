defmodule Peer.Handshake.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def listen(sock) do
    Supervisor.start_child(__MODULE__, [sock])
  end

  def connect(host, port, info_hash) do
    Supervisor.start_child(__MODULE__, [host, port, info_hash])
  end

  def init(:ok) do
    children = [
      worker(Peer.Handshake, [], restart: :temporary)
    ]

    supervise(children, strategy: :simple_one_for_one)
  end
end
