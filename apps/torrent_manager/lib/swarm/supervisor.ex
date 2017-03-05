defmodule Swarm.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @spec start_child(binary) :: :ok
  def start_child(info_hash) do
    child = worker(Swarm.Stats, [info_hash], id: {info_hash, :store}, restart: :transient)
    Supervisor.start_child(__MODULE__, child)

    child = worker(Swarm.PieceSet, [info_hash], id: {info_hash, :piece_set}, restart: :transient)
    Supervisor.start_child(__MODULE__, child)

    child = worker(Torrent.BlockManager, [info_hash], id: {info_hash, :block_manager}, restart: :transient)
    Supervisor.start_child(__MODULE__, child)

    :ok
  end

  def init(:ok) do
    supervise([], strategy: :one_for_one)
  end
end
