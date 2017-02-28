defmodule PeerManager do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Peer.Swarm.Registry, []),
      registry(:unique, Peer.Registry),
      worker(Peer.EventManager, []),
      supervisor(Peer.Handshake.Supervisor, []),
      supervisor(Peer.Connection.Supervisor, []),
      supervisor(Peer.Manager.Store.Registry, []),
      worker(Registry, [:unique, Peer.Manager.Registry]),
      supervisor(Peer.Manager.Store.Supervisor, []),
      supervisor(Peer.Manager.Supervisor, []),
    ]

    Supervisor.start_link(children, [strategy: :one_for_one])
  end

  defp registry(kind, registry) do
    Supervisor.Spec.supervisor(Registry, [kind, registry], id: registry)
  end

  def register(info_hash) do
    {:ok, _} = Peer.Manager.Store.Supervisor.start_child(info_hash)
    {:ok, _} = Peer.Manager.Supervisor.start_child(info_hash)
    :ok
  end

  def deregister(info_hash) do
    Peer.Registry.lookup(info_hash)
    |> Enum.each(&GenServer.stop(&1))

    :ok = Supervisor.terminate_child(Peer.Manager.Store.Supervisor, info_hash)
    :ok = Supervisor.delete_child(Peer.Manager.Store.Supervisor, info_hash)
    :ok = Supervisor.terminate_child(Peer.Manager.Supervisor, info_hash)
    :ok = Supervisor.delete_child(Peer.Manager.Supervisor, info_hash)
  end

  @spec add_peers(binary, [{String.t, integer}]) :: :ok
  def add_peers(info_hash, peers) do
    Peer.Manager.add_peers(info_hash, peers)
  end
end
