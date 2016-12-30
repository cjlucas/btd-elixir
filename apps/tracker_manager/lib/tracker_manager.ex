defmodule TrackerManager do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Tracker.Manager, [5]),
      supervisor(Tracker.EventManager, []),
      supervisor(Task.Supervisor, [[name: Tracker.Worker.Supervisor]])
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def add_event_handler(handler, opts) do
    Tracker.EventManager.add_handler(handler, opts)
  end
end
