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

    def shuffle_trackers(%{trackers: trackers} = entry) do
      %{entry | trackers: shuffle_trackers(trackers, [])}
    end
    defp shuffle_trackers([], acc), do: Enum.reverse(acc)
    defp shuffle_trackers([head | rest], acc) do
      shuffle_trackers(rest, [Enum.shuffle(head) | acc])
    end

    def reorder_trackers(%{trackers: trackers} = entry, new_head) do
      %{entry | trackers: reorder_trackers(trackers, new_head, [])}
    end
    defp reorder_trackers([], _new_head, acc) do
      Enum.reverse(acc)
    end
    defp reorder_trackers([tier | rest], new_head, acc) do
      acc = cond do
        new_head in tier ->
          l = [new_head | Enum.filter(tier, &(&1 != new_head))]
          [l | acc]
        true ->
          [tier | acc]
      end
      reorder_trackers(rest, new_head, acc)
    end

    def cancel_timer(%{timer: timer} = entry) when is_nil(timer), do: entry
    def cancel_timer(%{timer: timer} = entry) do
      {:ok, :cancel} = :timer.cancel(timer)
      %{entry | timer: nil}
    end
  end
  
  defmodule State do
    defstruct entries: %{}
  end

  def start_link do
    GenServer.start_link(@name, [], name: @name)
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

  def init([]) do
    Tracker.EventManager.subscribe(:received_response)
    {:ok, %State{}}
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
      |> Entry.shuffle_trackers

      {:reply, :ok, %{state | entries: Map.put(entries, info_hash, entry)}}
    end
  end

  def handle_call({:deregister, info_hash}, _from, %{entries: entries} = state) do
    ret = cond do
      Map.has_key?(entries, info_hash) ->
        Map.get(entries, info_hash) |> Entry.cancel_timer
        :ok
      true ->
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
      entries = Map.update!(entries, info_hash, fn entry -> 
        entry = entry
        |> Entry.cancel_timer
        |> Entry.reorder_trackers(url)

        {:ok, tref} = :timer.send_after(:timer.seconds(duration), {:timer_fired, info_hash})
        time = NaiveDateTime.utc_now |> NaiveDateTime.add(duration)
        %{entry | timer: tref, next_announce: time}
      end)

      {:noreply, %{state | entries: entries}}
    else
      {:noreply, state}
    end
  end

  defp handle_request(info_hash, stats, %{entries: entries} = state) do
    entries = Map.update!(entries, info_hash, fn entry -> 
      req = %Tracker.Dispatcher.Request{
        info_hash: info_hash,
        trackers: List.flatten(entry.trackers),
        peer_id: entry.peer_id,
        event: stats.event,
        uploaded: stats.uploaded,
        downloaded: stats.downloaded,
        left: stats.left
      }
      Tracker.Dispatcher.request(req)

      entry = Entry.cancel_timer(entry)
      %{entry | next_announce: NaiveDateTime.utc_now}
    end)
    {:noreply, %{state | entries: entries}}
  end
end
