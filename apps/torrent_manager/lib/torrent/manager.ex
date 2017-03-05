defmodule Torrent.Manager do
  defmodule State do
    defstruct info_hash: <<>>
  end

  def start_link(info_hash) do
    GenServer.start_link(__MODULE__, info_hash, name: via(info_hash))
  end

  def init(info_hash) do
    {:ok, %State{info_hash: info_hash}}
  end

  defp via(info_hash) do
    {:via, Registry, {Torrent.Manager.Registry, info_hash}}
  end
end
