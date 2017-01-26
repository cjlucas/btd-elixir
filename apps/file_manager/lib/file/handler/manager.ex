defmodule Torrent.FileHandler.Manager do
  use GenServer

  @name __MODULE__

  def start_link do
    GenServer.start_link(@name, :ok, name: @name)
  end

  def write(fpath, loc, data) do
    GenServer.call(@name, {:write, fpath, loc, data})
  end

  def read(fpath, loc, num_bytes) do
    GenServer.call(@name, {:read, fpath, loc, num_bytes})
  end

  def close(fpath) do
    GenServer.call(@name, {:close, fpath})
  end

  def init(:ok) do
    {:ok, []}
  end

  def handle_call({:write, fpath, loc, data}, from, state) do
    spawn_link(fn ->
      GenServer.reply(from, call_handler(fpath, :write, [loc, data]))
    end)

    {:noreply, state}
  end

  def handle_call({:read, fpath, loc, num_bytes}, from, state) do
    spawn_link(fn ->
      GenServer.reply(from, call_handler(fpath, :read, [loc, num_bytes]))
    end)

    {:noreply, state}
  end

  def handle_call({:close, fpath}, _from, state) do
    case Registry.lookup(Torrent.FileHandler.Registry, fpath) do
      [{pid, _}] ->
        GenServer.stop(pid, :normal)
      [] ->
        nil
    end
    {:reply, :ok, state}
  end

  defp lookup(fpath) do
    case Registry.lookup(Torrent.FileHandler.Registry, fpath) do
      [{pid, _}] ->
        {:ok, pid}
      [] ->
        open(fpath)
    end
  end

  defp open(fpath) do
    Torrent.FileHandler.Supervisor.start_child(fpath)
  end

  defp call_handler(fpath, fun, args) do
    case lookup(fpath) do
      {:ok, pid} ->
        try do
          apply(Torrent.FileHandler, fun, [pid | args])
        catch
          :exit, _ ->
            call_handler(fpath, fun, args)
        end
      {:error, reason} ->
        {:error, reason}
    end
  end
end
