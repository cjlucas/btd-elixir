#:observer.start
#Process.sleep(5_000)

{:ok, torrent} = File.read!("apps/torrent_manager/test/fixtures/ubuntu-16.10-desktop-amd64.iso.torrent")
                 |> Torrent.parse

:ok = TorrentManager.register(torrent, "/Users/chris/Downloads/blah")
:ok = TorrentManager.start(torrent.info_hash)

TrackerManager.subscribe(:received_response)

Process.sleep(:infinity)
