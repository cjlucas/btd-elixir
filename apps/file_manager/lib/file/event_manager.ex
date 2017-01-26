defmodule File.EventManager do
  def start_link do
    Registry.start_link(:duplicate, __MODULE__)
  end

  def register(info_hash) do
    Registry.register(__MODULE__, info_hash, [])
  end

  def deregister(info_hash) do
    Registry.unregister(__MODULE__, info_hash)
  end

  def write_failed(info_hash, error) do
    notify(info_hash, {:write_failed, info_hash, error})
  end

  def piece_completed(info_hash, piece_idx) do
    notify(info_hash, {:piece_completed, info_hash, piece_idx})
  end

  defp notify(key, msg) do
    Registry.dispatch(__MODULE__, key, fn entries ->
      for {pid, _} <- entries, do: send(pid, msg)
    end)
  end
end
