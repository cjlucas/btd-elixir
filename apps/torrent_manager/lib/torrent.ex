defmodule Torrent.File do
  defstruct path: "", size: 0
end

defmodule Torrent do
  defstruct info_hash: <<>>, piece_length: 0, pieces: [], private: false, trackers: [], files: []

  def parse(iodata) do
    case Bento.torrent(iodata) do
      %Bento.Metainfo.Torrent{} = t ->
        tor = %Torrent{
          info_hash: calc_info_hash(t),
          piece_length: Map.get(t.info, "piece length"),
          pieces: Map.get(t.info, "pieces"),
          private: Map.get(t.info, "private") == 1,
          trackers: [t.announce | Map.get(t, "announce-list") || []],
          files: build_file_list(t.info)
        }
        {:ok, tor}
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp calc_info_hash(%Bento.Metainfo.Torrent{info: info}) do
    # Remove any keys without an associated value by removing all
    # keys that have an associated falsey value
    info = Map.drop(info, Enum.filter(Map.keys(info), &(!info[&1])))

    {:ok, info} = Bento.encode(info)
    :crypto.hash(:sha, info)
  end

  defp build_file_list(%{"name" => name, "length" => size}) do
    [%Torrent.File{path: name, size: size}]
  end

  defp build_file_list(%Bento.Metainfo.MultiFile{files: files}) do
    files |> Enum.map(fn f ->
      %Torrent.File{path: f["path"], size: f["length"]}
    end) 
  end
end
