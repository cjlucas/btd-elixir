defmodule Peer.Manager.Store do

  defmodule Info do
    defstruct skey_hash: <<>>, peer_id: <<>>, uploaded: 0, downloaded: 0
  end

  defmodule State do
    defstruct stats: %{}, skey_map: %{}
  end

  def start_link do
    Agent.start_link(fn -> %State{} end, name: __MODULE__)
  end

  def reset do
    Agent.update(__MODULE__, fn _ -> %State{} end)
  end

  def add(info_hash) do
    Agent.update(__MODULE__, fn %{stats: stats, skey_map: skey_map} = state ->
      skey_hash = Peer.HandshakeUtils.req2(info_hash)
      info = %Info{
        skey_hash: skey_hash,
        peer_id: :crypto.strong_rand_bytes(20)
      }

      %{state |
        stats: Map.put(stats, info_hash, info),
        skey_map: Map.put(skey_map, skey_hash, info_hash)}
    end)
  end

  def remove(info_hash) do
    Agent.update(__MODULE__, fn %{stats: stats, skey_map: skey_map} = state ->
      %{state |
        stats: Map.delete(stats, info_hash),
        skey_map: Map.delete(skey_map, Peer.HandshakeUtils.req2(info_hash))}
    end)
  end

  def resolve_info_hash(skey_hash) do
    Agent.get(__MODULE__, fn %{skey_map: map} ->
      Map.get(map, skey_hash)
    end)
  end

  def lookup_peer_id(info_hash) do
    Agent.get(__MODULE__, fn %{stats: stats} ->
      case Map.get(stats, info_hash) do
        %{peer_id: peer_id} -> peer_id
        nil                 -> nil
      end
    end)
  end

  def stats(info_hash) do
    Agent.get(__MODULE__, fn %{stats: stats} ->
      case Map.get(stats, info_hash) do
        %{uploaded: up, downloaded: down} ->
          [uploaded: up, downloaded: down]
        nil ->
          nil
      end
    end)
  end

  def incr_uploaded(info_hash, amnt) do
    incr_val(info_hash, :uploaded, amnt)
  end

  def incr_downloaded(info_hash, amnt) do
    incr_val(info_hash, :downloaded, amnt)
  end

  defp incr_val(info_hash, key, amnt) do
    Agent.get_and_update(__MODULE__, fn %{stats: stats} = state ->
      stats = Map.update!(stats, info_hash, fn stat ->
        Map.update!(stat, key, &(&1 + amnt))
      end)

      {:ok, %{state | stats: stats}}
    end)
  end
end
