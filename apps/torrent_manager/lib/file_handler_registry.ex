defmodule Torrent.FileHandler.Manager do
  use GenServer

  @name __MODULE__
  @registry Torrent.FileHandler.Registry

  defmodule State do
    defstruct timers: MapSet.new
  end

  def start_link do
    GenServer.start_link(@name, :ok, name: @name)
  end

  def write(info_hash, fname, loc, data) do
    GenServer.cast(@name, {:write, info_hash, fname, loc, data})
  end

  def read(info_hash, fname, loc, num_bytes) do
    GenServer.call(@name, {:read, info_hash, fname, loc, num_bytes})
  end

  def stop_all(info_hash) do
    GenServer.call(@name, {:stop_all, info_hash})
  end

  def handle_call({:read, info_hash, fname, loc, bytes}) do
    
  end

  def handle_call({:stop_all, info_hash}) do
  end

  defp register(info_hash, fpath) do
    {:ok, pid} = Torrent.FileHandler.Supervisor.start_child(fpath)
    Registry.register(@registry, info_hash, {fname, pid})
    pid
  end

  defp lookup(info_hash, fpath) do
    case Registry.match(@registry, info_hash, {fpath, :_}) do
      [{_, {_, pid}}] ->
        pid
      [] ->
        register(info_hash, fpath)
    end
  end
end
