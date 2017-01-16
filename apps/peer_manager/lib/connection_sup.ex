defmodule Peer.Connection.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def start_child(info_hash, sock) do
    Supervisor.start_child(__MODULE__, [info_hash, sock])
  end

  def init(:ok) do
    children = [
      worker(Peer.Connection, [], restart: :temporary)
    ]

    supervise(children, strategy: :simple_one_for_one)
  end
end
