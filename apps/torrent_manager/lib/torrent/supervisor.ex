defmodule Torrent.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    children = [
      worker(Torrent.Store, []),
      registry(:unique, Torrent.Manager.Registry),
      supervisor(Torrent.Store.Supervisor, []),
      supervisor(Torrent.Manager.Supervisor, []),
      supervisor(Swarm.Supervisor, []),
      supervisor(Swarm.Registry, []),
      registry(:unique, Peer.Registry),
      worker(Swarm.EventManager, []),
      supervisor(Peer.Handshake.Supervisor, []),
      supervisor(Peer.Connection.Supervisor, []),
      supervisor(Swarm.Stats.Registry, []),
      registry(:unique, Peer.Manager.Registry),
      supervisor(Swarm.Manager.Supervisor, []),
      worker(TrackerEventHandler, []),
    ]

    supervise(children, strategy: :one_for_one)
  end

  defp registry(kind, registry) do
    Supervisor.Spec.supervisor(Registry, [kind, registry], id: registry)
  end
end
