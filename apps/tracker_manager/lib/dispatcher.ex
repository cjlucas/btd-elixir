defmodule Tracker.Dispatcher do
  use GenServer
  require Logger

  @name __MODULE__

  defmodule Request do
    defstruct info_hash: <<>>,
      trackers: [],
      peer_id: <<>>,
      event: nil,
      uploaded: 0,
      downloaded: 0,
      left: 0
  end

  defmodule State do
    defstruct slots: 1, queue: :queue.new()
  end

  def start_link(num_slots) do
    GenServer.start_link(@name, num_slots, name: @name)
  end

  def request(req) do
    GenServer.cast(@name, {:request, req})
  end

  def init(num_slots) do
    {:ok, %State{slots: num_slots}}
  end

  def handle_cast({:request, req}, %{slots: slots} = state) when slots > 0 do
    do_request(req)
    {:noreply, %{state | slots: slots-1}}
  end
  
  def handle_cast({:request, req}, %{slots: slots, queue: q} = state) when slots == 0 do
    {:noreply, %{state | queue: :queue.in(req, q)}}
  end
  
  def handle_info({:DOWN, _ref, :process, _pid, _reason}, %{queue: q} = state) do
    state = cond do
      state.slots == 0 && !:queue.is_empty(q) ->
        {{:value, req}, q} = :queue.out(q)
        Logger.debug("dequeued entry len(q) = #{:queue.len(q)}")
        do_request(req)
        %{state | queue: q}
      true ->
        %{state | slots: state.slots+1}
    end
        
    {:noreply, state}
  end

  def do_request(req) do
    url = req.trackers |> List.first |> List.first
    req = %Tracker.Request{
      url: url,
      info_hash: req.info_hash,
      peer_id: req.peer_id,
      bytes_uploaded: req.uploaded,
      bytes_downloaded: req.downloaded,
      bytes_left: req.left,
      event: req.event
    }

    {:ok, pid} = Task.Supervisor.start_child(Tracker.Worker.Supervisor, fn ->
      Logger.info("Sending request: #{inspect req}")
      {:ok, resp} = Tracker.request(req)
      Tracker.EventManager.received_response(req.info_hash, url, resp)
    end)

    Process.monitor(pid)
  end
end
