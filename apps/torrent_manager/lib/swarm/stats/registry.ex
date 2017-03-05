defmodule Swarm.Stats.Registry do
  def start_link do
    Registry.start_link(:unique, __MODULE__)
  end

  def register_skey_hash(skey_hash, info_hash) do
    Registry.register(__MODULE__, {:skey_hash, skey_hash}, info_hash)
  end

  def resolve_info_hash(skey_hash) do
    case Registry.lookup(__MODULE__, {:skey_hash, skey_hash}) do
      [{_, info_hash} | _] ->
        info_hash
      [] ->
        nil
    end
  end
end
