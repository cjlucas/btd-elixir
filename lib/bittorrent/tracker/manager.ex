defmodule Tracker.Manager do
  use GenServer
  require Logger

  @name __MODULE__

  defmodule State do
    defstruct slots: 0, trackers: %{}, queue: :queue.new(), timers: []
  end

  def start_link(slots) do
    GenServer.start_link(@name, [slots], name: @name)
  end

  def add(url, info_hash, peer_id) do
    GenServer.call(@name, {:add_tracker, url, info_hash, peer_id})
  end

  def remove(url) do
    GenServer.call(@name, {:remove_tracker, url})
  end

  def request(url) do
    GenServer.cast(@name, {:request, url})
  end

  defp register_timer(url, interval) do
    GenServer.call(@name, {:register_timer, url, interval})
  end

  def init([slots]) do
    {:ok, %State{slots: slots}}
  end

  def handle_call({:add_tracker, url, info_hash, peer_id}, _from, state) do
    new_state = update_in(state.trackers, &(Map.put(&1, url, {info_hash, peer_id})))
    {:reply, :ok, new_state}
  end

  def handle_call({:remove_tracker, url}, _from, state) do
    new_state = %{state | trackers: Map.delete(state.trackers, url)}
    {:reply, :ok, new_state}
  end

  def handle_cast({:request, url}, state = %State{slots: slots}) when slots > 0 do
    dispatch(url, state.trackers[url])
    {:noreply, %{state | slots: slots-1}}
  end
  
  def handle_cast({:request, url}, state = %State{slots: slots, queue: q}) when slots == 0 do
    {:noreply, %{state | queue: :queue.in(url, q)}}
  end

  def handle_call({:register_timer, url, duration}, _from, state = %State{timers: timers}) do
    tref = :timer.send_after(:timer.seconds(duration), {:timer_fired, url})
    {:reply, :ok, %{state | timers: [tref | timers]}}
  end

  def handle_info({:timer_fired, url}, state) do
    Logger.info("Timer fired for url #{url}")
    request(url)
    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, _pid, _reason}, state) do
    state = cond do
      state.slots == 0 && !:queue.is_empty(state.queue) ->
        {{:value, url}, q} = :queue.out(state.queue)
        dispatch(url, state.trackers[url])
        %{state | queue: q}
      true ->
        %{state | slots: state.slots+1}
    end
        
    {:noreply, state}
  end

  defp dispatch(url, {info_hash, peer_id}) do
    req = %Tracker.Request{url: url, info_hash: info_hash, peer_id: peer_id}
    {:ok, pid} = Task.Supervisor.start_child(Tracker.Worker.Supervisor, fn ->
      Logger.info("Sending request: #{inspect req}")
      {:ok, resp} = Tracker.request(req)
      Logger.info("Got response #{inspect resp}. Queuing request to fire after #{resp.interval} seconds")

      if resp.interval > 0, do: register_timer(url, resp.interval)
    end)

    Process.monitor(pid)
  end
end
