defmodule Tracker.Manager do
  use GenServer
  require Logger

  @name __MODULE__

  defmodule Entry do
    defstruct info_hash: <<>>, url: "", next_announce: nil, timer: nil, state: :waiting
  end
  
  defmodule State do
    defstruct slots: 0, entries: %{}, queue: :queue.new()
  end

  def start_link(slots) do
    GenServer.start_link(@name, [slots], name: @name)
  end

  def add(url, info_hash) do
    GenServer.call(@name, {:add_tracker, url, info_hash})
  end

  def remove(url, info_hash) do
    GenServer.call(@name, {:remove_tracker, url, info_hash})
  end

  def request(url, info_hash) do
    GenServer.cast(@name, {:request, url, info_hash})
  end

  defp register_timer(id, interval) do
    GenServer.call(@name, {:register_timer, id, interval})
  end

  def init([slots]) do
    {:ok, %State{slots: slots}}
  end

  def handle_call({:add_tracker, url, info_hash}, _from, %{entries: entries} = state) do
    if Map.has_key?(entries, info_hash) do
      {:reply, {:error, :already_exists}, state}
    else
      entry = %Entry{
        info_hash: info_hash,
        url: url,
      }

      {:reply, :ok, %{state | entries: Map.put(entries, entry_id(info_hash, url), entry)}}
    end
  end

  def handle_call({:remove_tracker, url, info_hash}, _from, %{entries: entries} = state) do
    id = entry_id(info_hash, url)
    {:reply, :ok, %{state | entries: Map.delete(entries, id)}}
  end

  def handle_call({:register_timer, id, duration}, _from, %{entries: entries} = state) do
    if Map.has_key?(entries, id) do
      entries = Map.get_and_update(entries, id, fn entry -> 
        {:ok, tref} = :timer.send_after(:timer.seconds(duration), {:timer_fired, id})
        time = NaiveDateTime.utc_now |> NaiveDateTime.add(duration)
        %{entry | timer: tref, next_announce: time, state: :waiting}
      end)

      {:reply, :ok, %{state | entries: entries}}
    else
      {:reply, :ok, state}
    end
  end
  
  def handle_cast({:request, url, info_hash}, %State{slots: slots, entries: entries} = state) when slots > 0 do
    id = entry_id(info_hash, url)
    entries = Map.get_and_update!(entries, id, fn %{timer: timer} = entry -> 
      if timer != nil do
        {:ok, :cancel}  = :timer.cancel(timer)
      end
      %{entry | state: :queued, timer: nil}
    end)
    {:noreply, %{state | entries: entries, slots: slots-1}}
  end

  def handle_cast({:request, url, info_hash}, state = %State{slots: slots, queue: q}) when slots == 0 do
    id = entry_id(info_hash, url)
    {:noreply, %{state | queue: :queue.in(id, q)}}
  end

  def handle_info({:timer_fired, id}, state) do
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

  defp entry_id(info_hash, url) do
    :crypto.hash(:sha, [info_hash, url])
  end

  defp dispatch(url, id) do
    req = %Tracker.Request{url: url, info_hash: info_hash, peer_id: peer_id}
    {:ok, pid} = Task.Supervisor.start_child(Tracker.Worker.Supervisor, fn ->
      Logger.info("Sending request: #{inspect req}")
      {:ok, resp} = Tracker.request(req)
      Tracker.EventManager.received_response(resp)
      Logger.info("Got response #{inspect resp}. Queuing request to fire after #{resp.interval} seconds")

      if resp.interval > 0, do: register_timer(url, resp.interval)
    end)

    Process.monitor(pid)
  end
end
