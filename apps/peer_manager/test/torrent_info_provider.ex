defmodule TorrentInfoProvider.Random do
  @info_hash Application.fetch_env!(:peer_manager, :test_info_hash)

  def resolve_info_hash(skey_hash) do
    IO.puts "OMGHERE"
    {:ok, @info_hash}
  end

  def resolve_peer_id(info_hash) do
    {:ok, :crypto.hash(:sha, ["bar", info_hash])}
  end
end
