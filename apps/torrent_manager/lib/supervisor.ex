defmodule Torrent.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def register(torrent) do
    {:ok, pid} = Torrent.Store.Supervisor.start_child(torrent)
    :ok = Torrent.Registry.register(torrent.info_hash, pid)
  end

  def init(:ok) do
    children = [
      worker(Torrent.Registry, []),
      supervisor(Torrent.Store.Supervisor, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
