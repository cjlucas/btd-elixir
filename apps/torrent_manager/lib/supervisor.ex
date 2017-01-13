defmodule Torrent.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    children = [
      worker(Torrent.Registry, []),
      supervisor(Torrent.Store.Supervisor, []),
      supervisor(Torrent.Manager.Supervisor, []),
      supervisor(Torrent.FileHandler.Supervisor, []),
      worker(Registry, [:unique, Torrent.FileHandler.Registry]),
      worker(Torrent.FileHandler.Manager, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
