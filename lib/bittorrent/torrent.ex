defmodule Torrent.Metainfo.File do
  defstruct path: "", size: 0
end

defmodule Torrent.Metainfo do
  defstruct piece_length: 0, pieces: [], private: false, trackers: [], files: []
end

defmodule Torrent do
  defstruct info_hash: <<>>, peer_id: <<>>, metainfo: %Torrent.Metainfo{}, root: "", uploaded: 0, downloaded: 0, state: :stopped
end
