#:observer.start
#Process.sleep(5_000)

info_hash = <<4, 3, 251, 71, 40, 189, 120, 143, 188, 182, 126, 135, 214, 254, 178, 65, 239, 56, 199, 90>>
url = "http://torrent.ubuntu.com:6969/announce"

{:ok, torrent} = File.read!("apps/torrent_manager/test/fixtures/ubuntu-16.10-desktop-amd64.iso.torrent")
                 |> Torrent.parse

IO.puts("num pieces: #{length(torrent.pieces)}")

{:ok, _} = Torrent.Registry.register(torrent)

files = torrent.files |> Enum.map(fn %{path: path, size: size} -> {path, size} end)
:ok = FileManager.register(info_hash,
                           "/Users/chris/Downloads",
                           files,
                           torrent.pieces,
                           torrent.piece_length)

Peer.Manager.Store.add(info_hash)
{:ok, _} = Peer.Manager.Supervisor.start_child(info_hash)

TrackerManager.subscribe(:received_response)
:ok = Tracker.Manager.register(info_hash, info_hash, [[url]])
:ok = Tracker.Manager.request(info_hash, :started, 0, 0, 0)


receive do
  {:received_response, info_hash, url, resp} ->
    host = System.argv |> Enum.at(0)
    port = System.argv |> Enum.at(1) |> String.to_integer
    IO.puts("host: #{host} port: #{port}")
    Peer.Handshake.Supervisor.connect(host, port, info_hash)
    Process.sleep(:infinity)
after
  5000 -> IO.puts("timed out")
end
