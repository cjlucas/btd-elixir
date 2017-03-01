#:observer.start
#Process.sleep(5_000)

{:ok, torrent} = File.read!("apps/torrent_manager/test/fixtures/ubuntu-16.10-desktop-amd64.iso.torrent")
                 |> Torrent.parse

:ok = TorrentManager.register(torrent, "/Users/chris/Downloads")
:ok = TorrentManager.start(torrent.info_hash)

TrackerManager.subscribe(:received_response)

receive do
  {:received_response, info_hash, url, resp} ->
    host = System.argv |> Enum.at(0)
    port = System.argv |> Enum.at(1) |> String.to_integer
    IO.puts("host: #{host} port: #{port}")
    Peer.Handshake.Supervisor.connect(host, port, info_hash)

    #:ok = PeerManager.add_peers(torrent.info_hash, resp.peers)
    Process.sleep(:infinity)
after
  5000 -> IO.puts("timed out")
end
