defmodule Peer.Stats do
  defstruct uploaded: 0, downloaded: 0
end

defmodule Peer.Stats.Store do
  alias Peer.Stats

  defmodule State do
    defstruct stats: %{}
  end

  def start_link do
    Agent.start_link(fn -> %State{} end, name: __MODULE__)
  end

  def reset do
    Agent.update(__MODULE__, fn _ -> %State{} end)
  end

  def add(info_hash) do
    Agent.update(__MODULE__, fn %{stats: stats} = state ->
      %{state | stats: Map.put(stats, info_hash, %Stats{})}
    end)
  end
  
  def remove(info_hash) do
    Agent.update(__MODULE__, fn %{stats: stats} = state ->
      %{state | stats: Map.delete(stats, info_hash)}
    end)
  end

  def stats(info_hash) do
    Agent.get(__MODULE__, fn %{stats: stats} ->
      Map.get(stats, info_hash)
    end)
  end

  def incr_uploaded(info_hash, amnt) do
    incr_val(info_hash, :uploaded, amnt)
  end
  
  def incr_downloaded(info_hash, amnt) do
    incr_val(info_hash, :downloaded, amnt)
  end

  defp incr_val(info_hash, key, amnt) do
    Agent.get_and_update(__MODULE__, fn %{stats: stats} = state ->
      stats = Map.update!(stats, info_hash, fn stat ->
        Map.update!(stat, key, &(&1 + amnt))
      end)

      {Map.get(stats, info_hash), %{state | stats: stats}}
    end)
  end
end
