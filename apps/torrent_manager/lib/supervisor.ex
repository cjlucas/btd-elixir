defmodule Torrent.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    children = [
      worker(Torrent.Store, []),
      supervisor(Registry, [:unique, Torrent.Manager.Registry]),
      supervisor(Torrent.Store.Supervisor, []),
      supervisor(Torrent.Manager.Supervisor, []),
    ]

    supervise(children, strategy: :one_for_one)
  end
end
