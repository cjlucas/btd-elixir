defmodule FileManager do
  @moduledoc """
  Documentation for FileManager.
  """

  @block_size 16384

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(File.Manager.Supervisor, []),
      supervisor(Registry, [:unique, File.Manager.Registry], id: File.Manager.Registry),
      supervisor(Torrent.FileHandler.Supervisor, []),
      worker(File.Manager.Store, []),
      worker(Registry, [:unique, Torrent.FileHandler.Registry]),
      worker(Torrent.FileHandler.Manager, [])
    ]

    Supervisor.start_link(children, [strategy: :one_for_one])
  end

  def register(info_hash, root, files, piece_hashes, piece_size) do
    :ok = File.Manager.Store.add(info_hash, root, files, piece_hashes, piece_size, @block_size)
    {:ok, _} = File.Manager.Supervisor.start_child(info_hash)
    :ok
  end

  def deregister(info_hash) do
    with :ok <- File.Manager.Store.remove(info_hash),
         :ok <- Supervisor.terminate_child(File.Manager.Supervisor, info_hash),
         :ok <- Supervisor.delete_child(File.Manager.Supervisor, info_hash), do: :ok
  end

  def write_block(info_hash, piece_idx, offset, data) do
    File.Manager.write_block(info_hash, piece_idx, offset, data)
  end

  def read_block(info_hash, piece_idx, offset, size) do
    File.Manager.read_block(info_hash, piece_idx, offset, size)
  end
end
