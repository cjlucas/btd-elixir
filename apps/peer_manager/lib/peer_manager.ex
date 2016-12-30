defmodule PeerManager do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Peer.Registry, []),
      supervisor(Peer.Handshake.Supervisor, []),
      supervisor(Peer.Connection.Supervisor, []),
    ]

    Supervisor.start_link(children, [strategy: :one_for_one])
  end
end
