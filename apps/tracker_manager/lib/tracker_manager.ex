defmodule TrackerManager do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Tracker.EventManager, []),
      supervisor(Task.Supervisor, [[name: Tracker.Worker.Supervisor]]),
      worker(Tracker.Dispatcher, [5]),
      worker(Tracker.Manager, [])
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def subscribe(event) do
    Tracker.EventManager.subscribe(event)
  end
end
