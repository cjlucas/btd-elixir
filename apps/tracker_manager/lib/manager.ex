defmodule Tracker.Manager do
  use GenServer
  require Logger

  @name __MODULE__

  @default_interval 1800

  defmodule Stats do
    defstruct event: nil, uploaded: 0, downloaded: 0, left: 0
  end

  defmodule Entry do
    defstruct trackers: [], peer_id: <<>>, next_announce: nil, timer: nil
  end
  
  defmodule State do
    defstruct slots: 0, entries: %{}
  end

  def start_link(slots) do
    GenServer.start_link(@name, [slots], name: @name)
  end

  def register(info_hash, peer_id, trackers) do
    GenServer.call(@name, {:register, info_hash, peer_id, trackers})
  end

  def deregister(info_hash) do
    GenServer.call(@name, {:deregister, info_hash})
  end

  def request(info_hash) do
    GenServer.cast(@name, {:request, info_hash})
  end

  def request(info_hash, event, uploaded, downloaded, left) do
    GenServer.cast(@name, {:request, info_hash, event, uploaded, downloaded, left})
  end

  def init([slots]) do
    Tracker.EventManager.subscribe(:received_response)
    {:ok, %State{slots: slots}}
  end

  def handle_call({:register, info_hash, peer_id, trackers}, _from, %{entries: entries} = state) do
    if Map.has_key?(entries, info_hash) do
      {:reply, {:error, :already_exists}, state}
    else
      entry = %Entry{
        trackers: trackers,
        peer_id: peer_id,
        next_announce: NaiveDateTime.utc_now
      }

      {:reply, :ok, %{state | entries: Map.put(entries, info_hash, entry)}}
    end
  end

  def handle_call({:remove_tracker, info_hash}, _from, %{entries: entries} = state) do
    ret = case Map.get(entries, info_hash) do
      %{timer: timer} ->
        if !is_nil(timer) do
          {:ok, :cancel} = :timer.cancel(timer)
        end
        :ok
      nil ->
        {:error, :not_found}
    end
    {:reply, ret, %{state | entries: Map.delete(entries, info_hash)}}
  end

  def handle_cast({:request, info_hash}, state) do
    handle_request(info_hash, %Stats{}, state)
  end

  def handle_cast({:request, info_hash, event, uploaded, downloaded, left}, state) do
    stats = %Stats{event: event, uploaded: uploaded, downloaded: downloaded, left: left}
    handle_request(info_hash, stats, state)
  end

  def handle_info({:timer_fired, info_hash}, %{entries: entries} = state) do
    Logger.info("Timer fired for info_hash #{inspect info_hash}")

    if !Map.has_key?(entries, info_hash) do
      raise "timer fired on deregistered torrent"
    end

    handle_cast({:request, info_hash}, state)
  end

  # EventManager handlers

  def handle_info({:received_response, info_hash, url, resp}, %{entries: entries} = state) do
    duration = if resp.interval == 0, do: @default_interval, else: resp.interval
    Logger.info("Got response #{inspect resp}. Queuing request to fire after #{duration} seconds")

    if Map.has_key?(entries, info_hash) do
      entries = Map.update!(entries, info_hash, fn %{timer: timer} = entry -> 
        if !is_nil(timer) do
          {:ok, :cancel} = :timer.cancel(timer)
        end

        {:ok, tref} = :timer.send_after(:timer.seconds(duration), {:timer_fired, info_hash})
        time = NaiveDateTime.utc_now |> NaiveDateTime.add(duration)
        %{entry | timer: tref, next_announce: time}
      end)

      {:reply, :ok, %{state | entries: entries}}
    else
      {:reply, :ok, state}
    end
  end

  defp handle_request(info_hash, stats, %{slots: slots, entries: entries} = state) when slots > 0 do
    # NOTE(clucas): Crash here if torrent was deregistered. Case where
    # torrent is deregistered while in the queue happens on dequeue.
    entries = Map.update!(entries, info_hash, fn %{timer: timer} = entry -> 
      if timer != nil do
        {:ok, :cancel} = :timer.cancel(timer)
      end
      dispatch(info_hash, entry, stats)
      %{entry | timer: nil}
    end)
    {:noreply, %{state | entries: entries, slots: slots-1}}
  end

  defp handle_request(info_hash, stats, %{slots: slots, entries: entries, queue: q} = state) when slots == 0 do
    Logger.debug("queueing entry len(q) = #{:queue.len(q)}")
    {:noreply, %{state | entries: entries, queue: :queue.in({info_hash, stats}, q)}}
  end

  def dispatch(info_hash, entry, stats) do
    req = %Tracker.Dispatcher.Request{
      info_hash: info_hash,
      trackers: entry.trackers,
      peer_id: entry.peer_id,
      event: stats.event,
      uploaded: stats.uploaded,
      downloaded: stats.downloaded,
      left: stats.left
    }

    Tracker.Dispatcher.request(req)
  end
end
