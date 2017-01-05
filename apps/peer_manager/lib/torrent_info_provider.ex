defmodule TorrentInfoProvider.TorrentManager do
  def resolve_info_hash(skey_hash) do
    case TorrentManager.lookup(:skey_hash, skey_hash) do
      {:ok, %{torrent: %Torrent{info_hash: hash}}} ->
        {:ok, hash}
      {:error, reason} ->
        {:error, reason}
    end
  end

  def resolve_peer_id(info_hash) do
    case TorrentManager.lookup(:info_hash, info_hash) do
      {:ok, %{peer_id: peer_id}} ->
        {:ok, peer_id}
      {:error, reason} ->
        {:error, reason}
    end
  end
end

defmodule TorrentInfoProvider.Random do
  @info_hash Application.get_env(:peer_manager, :test_info_hash)

  def resolve_info_hash(skey_hash) when is_nil(@info_hash) do
    raise "test_info_hash is not set"
  end

  def resolve_info_hash(skey_hash) do
    {:ok, @info_hash}
  end

  def resolve_peer_id(info_hash) do
    {:ok, :crypto.hash(:sha, ["bar", info_hash])}
  end
end
