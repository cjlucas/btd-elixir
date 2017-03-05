defmodule PeerManager do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Swarm.Registry, []),
      registry(:unique, Peer.Registry),
      worker(Swarm.EventManager, []),
      supervisor(Peer.Handshake.Supervisor, []),
      supervisor(Peer.Connection.Supervisor, []),
      supervisor(Swarm.Stats.Registry, []),
      worker(Registry, [:unique, Peer.Manager.Registry]),
      supervisor(Swarm.Stats.Supervisor, []),
      supervisor(Swarm.Manager.Supervisor, []),
    ]

    Supervisor.start_link(children, [strategy: :one_for_one])
  end

  defp registry(kind, registry) do
    Supervisor.Spec.supervisor(Registry, [kind, registry], id: registry)
  end

  def register(info_hash) do
    {:ok, _} = Swarm.Stats.Supervisor.start_child(info_hash)
    {:ok, _} = Swarm.Manager.Supervisor.start_child(info_hash)
    :ok
  end

  def deregister(info_hash) do
    Peer.Registry.lookup(info_hash)
    |> Enum.each(&GenServer.stop(&1))

    :ok = Supervisor.terminate_child(Swarm.Stats.Supervisor, info_hash)
    :ok = Supervisor.delete_child(Swarm.Stats.Supervisor, info_hash)
    :ok = Supervisor.terminate_child(Swarm.Manager.Supervisor, info_hash)
    :ok = Supervisor.delete_child(Swarm.Manager.Supervisor, info_hash)
  end

  @spec add_peers(binary, [{String.t, integer}]) :: :ok
  def add_peers(info_hash, peers) do
    Swarm.Manager.add_peers(info_hash, peers)
  end
end
