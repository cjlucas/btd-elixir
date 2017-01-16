info_hash = <<4, 3, 251, 71, 40, 189, 120, 143, 188, 182, 126, 135, 214, 254, 178, 65, 239, 56, 199, 90>>
url = "http://torrent.ubuntu.com:6969/announce"

{:ok, torrent} = File.read!("apps/torrent_manager/test/fixtures/ubuntu-16.10-desktop-amd64.iso.torrent")
                 |> Torrent.parse

IO.puts("num pieces: #{length(torrent.pieces)}")

{:ok, _} = Torrent.Registry.register(torrent)

TrackerManager.subscribe(:received_response)
:ok = Tracker.Manager.register(info_hash, info_hash, [[url]])
:ok = Tracker.Manager.request(info_hash, :started, 0, 0, 0)

receive do
  {:received_response, info_hash, url, resp} ->
    host = "192.168.1.11"
    port = 64155
    IO.puts("host: #{host} port: #{port}")
    Peer.Handshake.Supervisor.connect(host, port, info_hash)
    Process.sleep(2 * 60_000)
after
  5000 -> IO.puts("timed out")
end
