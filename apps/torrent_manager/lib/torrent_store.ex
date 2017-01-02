defmodule Torrent.Store do
  use GenServer

  defmodule State do
    defstruct torrent: nil, uploaded: 0, downloaded: 0 
  end

  def start_link(torrent) do
    GenServer.start_link(__MODULE__, torrent)
  end

  def incr_upload(pid, incr_amnt) when is_number(incr_amnt) do
    GenServer.call(pid, {:incr_upload, incr_amnt})
  end
  
  def incr_download(pid, incr_amnt) when is_number(incr_amnt) do
    GenServer.call(pid, {:incr_download, incr_amnt})
  end

  def init(torrent) do
    {:ok, %State{torrent: torrent}}
  end

  def handle_call({:incr_upload, amnt}, _from, %{uploaded: uploaded} = state) do
    {:reply, :ok, %{state | uploaded: uploaded + amnt}} 
  end
  
  def handle_call({:incr_download, amnt}, _from, %{downloaded: downloaded} = state) do
    {:reply, :ok, %{state | downloaded: downloaded + amnt}} 
  end
end
