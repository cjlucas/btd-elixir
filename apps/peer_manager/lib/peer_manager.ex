defmodule PeerManager do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Peer.Stats.Store, []),
      worker(Peer.Registry, []),
      worker(Peer.EventManager, []),
      supervisor(Peer.Handshake.Supervisor, []),
      supervisor(Peer.Connection.Supervisor, []),
      worker(Registry, [:unique, Peer.Manager.Registry]),
      supervisor(Peer.Manager.Supervisor, []),
    ]

    Supervisor.start_link(children, [strategy: :one_for_one])
  end
end
