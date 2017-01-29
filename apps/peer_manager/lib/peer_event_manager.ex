defmodule Peer.EventManager do

  @name __MODULE__

  def start_link do
    Registry.start_link(:duplicate, @name)
  end

  def register(info_hash) do
  	Registry.register(@name, info_hash, [])
  end

  def deregister(info_hash) do
    Registry.unregister(@name, info_hash)
  end

  def peer_connected(info_hash, peer_id) do
    notify(info_hash, {:peer_connected, peer_id})
  end

  def peer_disconnected(info_hash, peer_id) do
    notify(info_hash, {:peer_disconnected, peer_id})
  end

  def received_message(info_hash, {peer_id, msg}) do
    notify(info_hash, {:received_message, peer_id, msg})
  end

  def sent_message(info_hash, {peer_id, msg}) do
    notify(info_hash, {:sent_message, peer_id, msg})
  end

  defp notify(info_hash, msg) do
    Registry.dispatch(@name, info_hash, fn entries ->
      for {pid, _} <- entries, do: send(pid, msg)
    end)
    :ok
  end
end
