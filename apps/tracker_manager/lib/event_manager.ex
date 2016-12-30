defmodule Tracker.EventManager do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def add_handler(handler, opts) do
    Supervisor.start_child(__MODULE__, [handler, opts])
  end

  def received_response(resp) do
    notify({:received_response, resp})
  end

  def init(:ok) do
    child = worker(GenServer, [], restart: :temporary)
    supervise([child], strategy: :simple_one_for_one)
  end

  defp notify(event) do
    for {_, pid, _, _} <- Supervisor.which_children(__MODULE__) do
      GenServer.cast(pid, event)
    end

    :ok
  end
end
