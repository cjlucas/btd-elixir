defmodule TorrentManager do
  use Application

  def start(_type, _args) do
    Torrent.Supervisor.start_link
  end

  @spec register(Torrent.t, String.t) :: :ok | {:error, :already_registered}
  def register(t, root) do
    %{info_hash: info_hash, files: files, pieces: pieces, piece_length: piece_len} = t
    files = files |> Enum.map(fn %{path: path, size: size} -> {path, size} end)

    :ok = Torrent.Store.add(t)
    :ok = FileManager.register(info_hash, root, files, pieces, piece_len)
    :ok = PeerManager.register(info_hash)
  end

  @spec start(binary) :: :ok | {:error, :already_started}
  def start(info_hash) do
    %{info_hash: info_hash, trackers: trackers} = Torrent.Store.get(info_hash)
    :ok = Tracker.Manager.register(info_hash, info_hash, trackers)
    :ok = Tracker.Manager.request(info_hash, :started, 0, 0, 0)
  end
end
