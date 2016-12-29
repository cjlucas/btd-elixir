defmodule Tracker.Supervisor do
  use Supervisor

  @name __MODULE__

  def start_link do
    Supervisor.start_link(@name, :ok, name: @name)
  end

  def init(:ok) do
    children = [
      worker(Tracker.Manager, [5]),
      supervisor(Task.Supervisor, [[name: Tracker.Worker.Supervisor, restart: :transient]])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
