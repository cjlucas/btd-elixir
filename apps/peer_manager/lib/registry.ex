defmodule Peer.Registry do
  def start_link do
    Registry.start_link(:duplicate, __MODULE__)
  end

  def register(info_hash, {host, port}) do
    Registry.register(__MODULE__, info_hash, {host, port})
  end
end
