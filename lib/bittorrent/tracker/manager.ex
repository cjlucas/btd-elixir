defmodule Tracker.Manager do
  use GenServer

  @name __MODULE__

  defmodule State do
    defstruct slots: 0, trackers: %{}, queue: :queue.new(), timers: []
  end

  def start_link(slots) do
    GenServer.start_link(@name, [slots], name: @name)
  end

  def add(url, peer_id) do
    GenServer.call(@name, {:add_tracker, url, peer_id})
  end

  def remove(url) do
    GenServer.call(@name, {:remove_tracker, url})
  end

  def request(url) do
    GenServer.cast(@name, {:request, url})
  end

  def init([slots]) do
    {:ok, %State{slots: slots}}
  end

  def handle_call({:add_tracker, url, peer_id}, _from, state) do
    new_state = update_in(state.trackers, &(Map.put(&1, url, peer_id)))
    {:reply, :ok, new_state}
  end

  def handle_call({:remove_tracker, url}, _from, state) do
    new_state = %{state | trackers: Map.delete(state.trackers, url)}
    {:reply, :ok, new_state}
  end

  def handle_cast({:request, url}, state = %State{slots: slots}) when slots > 0 do
    dispatch(url)
    {:noreply, %{state | slots: slots-1}}
  end
  
  def handle_cast({:request, url}, state = %State{slots: slots, queue: q}) when slots == 0 do
    {:noreply, %{state | queue: :queue.in(url, q)}}
  end

  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    state = cond do
      state.slots == 0 && !:queue.is_empty(state.queue) ->
        {{:value, url}, q} = :queue.out(state.queue)
        dispatch(url)
        %{state | queue: q}
      true ->
        %{state | slots: state.slots+1}
    end
        
    {:noreply, state}
  end

  defp dispatch(url) do
    {:ok, pid} = Task.start(fn -> :timer.sleep(10000); IO.puts("DISPATCHED #{url}") end)
    Process.monitor(pid)
    IO.puts("DISPATCHING #{url}")
  end
end
