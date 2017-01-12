defmodule Torrent.Manager do
  defmodule State do
    defstruct info_hash: <<>>
  end

  def start_link(info_hash) do
    GenServer.start_link(__MODULE__, info_hash)
  end

  def init(info_hash) do
    {:ok, %State{info_hash: info_hash}}
  end
end
