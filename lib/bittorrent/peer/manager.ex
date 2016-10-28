defmodule Peer.Manager do
  use GenServer

  @name __MODULE__

  defmodule State do
    defstruct []
  end

  def start_link do
    GenServer.start_link(@name, :ok, name: @name)
  end

  def init(:ok) do
    {:ok, %State{}}
  end
end
