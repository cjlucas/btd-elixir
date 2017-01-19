defmodule FileManager do
  @moduledoc """
  Documentation for FileManager.
  """

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Registry, [:unique, File.Manager.Registry], id: File.Manager.Registry),
      supervisor(Torrent.FileHandler.Supervisor, []),
      worker(Registry, [:unique, Torrent.FileHandler.Registry]),
      worker(Torrent.FileHandler.Manager, [])
    ]

    Supervisor.start_link(children, [strategy: :one_for_one])
  end
end
