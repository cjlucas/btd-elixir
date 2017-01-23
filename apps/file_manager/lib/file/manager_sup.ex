defmodule File.Manager.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def start_child(info_hash) do
    child = worker(File.Manager, [info_hash], id: info_hash, restart: :transient)
    Supervisor.start_child(__MODULE__, child)
  end

  def init(:ok) do
    supervise([], strategy: :one_for_one)
  end
end
