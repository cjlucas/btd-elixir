defmodule Torrent.Store do
  use GenServer

  @name __MODULE__

  defmodule State do
    defstruct torrents: %{}
  end

  def start_link do
    GenServer.start_link(@name, :ok, name: @name)
  end

  def register(torrent = %Bento.Metainfo.Torrent{}, root) do
    GenServer.call(@name, {:register, torrent, root})
  end

  def lookup(:info_hash, << hash :: bytes-size(20) >>) do
     GenServer.call(@name, {:lookup, :info_hash, hash})
  end

  def lookup(:skey_hash, << hash :: bytes-size(20) >>) do
    GenServer.call(@name, {:lookup, :skey_hash, hash})
  end

  def torrents do
    GenServer.call(@name, :torrents)
  end

  def init(:ok) do
    {:ok, %State{}}
  end

  def handle_call(:torrents, _from, state), do: {:reply, Map.values(state.torrents), state}

  def handle_call({:register, torrent, root}, _from, state) do
    info_hash = calc_info_hash(torrent)

    t = %Torrent{
      info_hash: info_hash,
      skey_hash: :crypto.hash(:sha, <<"req2">> <> info_hash),
      peer_id: :crypto.strong_rand_bytes(20),
      root: root,
      state: :stopped,
      metainfo: %Torrent.Metainfo{
        piece_length: Map.get(torrent.info, "piece length"),
        pieces: torrent.info.pieces,
        private: torrent.info.private == 1,
        trackers: [torrent.announce | Map.get(torrent, "announce-list") || []],
        files: build_file_list(torrent.info),
      }
    }
    
    cond do
      Map.has_key?(state.torrents, t.info_hash) ->
        {:reply, {:error, :torrent_exists}, state}
      true ->
        {:reply, :ok, %{state | torrents: Map.put(state.torrents, t.info_hash, t)}}
    end
  end

  def handle_call({:lookup, info_hash}, _from, state = %State{torrents: torrents}) do
    case torrents[info_hash] do
      nil ->
        {:reply, {:error, :not_found}}
      t ->
        {:reply, {:ok, t}}
    end
  end

  defp build_file_list(%Bento.Metainfo.SingleFile{name: name, length: size}) do
    [%Torrent.Metainfo.File{path: name, size: size}]
  end

  defp build_file_list(%Bento.Metainfo.MultiFile{files: files}) do
    files |> Enum.map(fn f ->
      %Torrent.Metainfo.File{path: f["path"], size: f["length"]}
    end) 
  end

  defp calc_info_hash(%Bento.Metainfo.Torrent{info: info}) do
    info = Map.from_struct(info)

    # Remove any keys without an associated value by removing all
    # keys that have an associated falsey value
    info = Map.drop(info, Enum.filter(Map.keys(info), &(!info[&1])))

    {:ok, info} = Bento.encode(info)
    :crypto.hash(:sha, info)
  end
end
