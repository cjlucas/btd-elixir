# TODO: rename this file

defmodule Torrent.FileHandler do
  use GenServer
  require Logger

  defmodule State do
    defstruct file: nil, timeout: 0
  end

  def start_link(fpath, timeout \\ 5000) do
    GenServer.start_link(__MODULE__, {fpath, timeout}) 
  end

  def init({fpath, timeout}) do
    case :file.open(fpath, [:read, :write, :binary, :raw]) do
      {:ok, file} ->
        {:ok, %State{file: file, timeout: timeout}, timeout}
      {:error, reason} ->
        {:stop, reason}
    end
  end

  def read(pid, loc, num_bytes) do
    GenServer.call(pid, {:read, loc, num_bytes})
  end

  def write(pid, loc, data) do
    GenServer.cast(pid, {:write, loc, data})
  end

  def handle_call({:read, loc, num_bytes}, _from, %{file: f, timeout: timeout} = state) do
    reply = :file.pread(f, loc, num_bytes)
    {:reply, reply, state, timeout}
  end

  def handle_cast({:write, loc, data}, %{file: f, timeout: timeout} = state) do
    :ok = :file.pwrite(f, loc, data)
    {:noreply, state, timeout}
  end

  def handle_info(:timeout, state) do
    {:stop, :normal, state}
  end

  def terminate(_reason, %{file: f}) do
    :file.close(f)
  end
end
