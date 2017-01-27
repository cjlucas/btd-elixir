defmodule PeerManager do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Peer.Manager.Store, []),
      worker(Peer.Registry, []),
      worker(Peer.EventManager, []),
      supervisor(Peer.Handshake.Supervisor, []),
      supervisor(Peer.Connection.Supervisor, []),
      worker(Registry, [:unique, Peer.Manager.Registry]),
      supervisor(Peer.Manager.Supervisor, []),
    ]

    Supervisor.start_link(children, [strategy: :one_for_one])
  end

  def register(info_hash) do
    {:ok, pid} = Peer.Manager.Supervisor.start_child(info_hash)
    :ok = Peer.Manager.Store.add(info_hash)
  end

  def deregister(info_hash) do
    Peer.Registry.lookup(info_hash)
    |> Enum.each(&GenServer.stop(&1))

    :ok = Peer.Manager.Store.remove(info_hash)
    :ok = Supervisor.terminate_child(Peer.Manager.Supervisor, info_hash)
    :ok = Supervisor.delete_child(Peer.Manager.Supervisor, info_hash)
  end
end
