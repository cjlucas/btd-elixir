defmodule Torrent.Store.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def start_child(torrent) do
    Supervisor.start_child(__MODULE__, [torrent])
  end

  def init(:ok) do
    children = [
      worker(Torrent.Store, [], restart: :temporary)
    ]

    supervise(children, strategy: :simple_one_for_one)
  end
end
