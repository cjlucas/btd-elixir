use Mix.Config
config :peer_manager, torrent_info_provider: TorrentInfoProvider.Random
config :peer_manager, test_info_hash: <<
    0x94, 0x11, 0x2C, 0x62, 0x5A,
    0x02, 0xBA, 0x80, 0x5A, 0xD0,
    0xE0, 0x92, 0x87, 0xF4, 0xA7,
    0xFC, 0xEF, 0xA8, 0x29, 0x1B
    >>