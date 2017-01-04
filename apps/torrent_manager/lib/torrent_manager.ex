defmodule TorrentManager do
  use Application

  def start(_type, _args) do
    Torrent.Supervisor.start_link
  end

  def lookup(method, key) when method in [:info_hash, :skey_hash] do
    case Torrent.Registry.lookup({method, key}) do
      {:ok, {store_pid, _}} ->
        {:ok, Torrent.Store.state(store_pid)}
      {:error, reason} ->
        {:error, reason}
    end
  end
end
