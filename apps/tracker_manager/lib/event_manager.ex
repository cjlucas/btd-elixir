defmodule Tracker.EventManager do

  @name __MODULE__

  @valid_keys [:received_response]

  def start_link do
    Registry.start_link(:unique, @name)
  end

  def subscribe(keys) when is_list(keys) do
    for key <- keys, do: subscribe(key)
  end

  def subscribe(key) when key in @valid_keys do
    Registry.register(@name, key, [])
  end

  def received_response(info_hash, url, resp) do
    notify(:received_response, {info_hash, url, resp})
  end

  defp notify(key, val) do
    Registry.dispatch(@name, key, fn entries ->
      for {pid, _} <- entries, do: send(pid, {key, val})
    end)
  end
end
