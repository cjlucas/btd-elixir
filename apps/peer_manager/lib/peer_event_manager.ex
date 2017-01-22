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

  def received_connection(info_hash, conn) do
    notify(info_hash, {:received_connection, conn})
  end

  def received_message(info_hash, {conn, msg}) do
    notify(info_hash, {:received_message, conn, msg})
  end

  def sent_message(info_hash, {conn, msg}) do
    notify(info_hash, {:sent_message, conn, msg})
  end

  defp notify(info_hash, msg) do
    Registry.dispatch(@name, info_hash, fn entries ->
      for {pid, _} <- entries, do: send(pid, msg)
    end)
    :ok
  end
end
