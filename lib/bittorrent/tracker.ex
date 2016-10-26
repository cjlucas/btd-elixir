defmodule Tracker do
  defmodule Request do
    # Tracker.Manager is responsible for setting event
    defstruct url: "", info_hash: <<>>, peer_id: <<>>, bytes_uploaded: 0, bytes_downloaded: 0, bytes_left: 0, event: nil, port: 0
  end

  defmodule Response do
    defstruct failure_reason: nil, tracker_id: nil, interval: 0, num_seeders: 0, num_leechers: 0, peers: []
  end

  def request(req = %Request{}) do
    uri = URI.parse(req.url)
    case uri.scheme do
      "http" ->
        params = %{
          "info_hash" => req.info_hash,
          "peer_id" => req.peer_id,
          "uploaded" => req.bytes_uploaded,
          "downloaded" => req.bytes_downloaded,
          "left" => req.bytes_left,
          "event" => req.event,
          "port" => req.port,
          "compact" => 1,
        }

        params = Map.merge(URI.decode_query(uri.query || ""), params) 
        with {:ok, resp} = HTTPoison.get(%{uri | query: URI.encode_query(params)}),
             {:ok, data} = Bento.decode(resp.body),
             do: {:ok, handle_http_response(data)}
    end
  end

  defp parse_binary_peers(buf), do: parse_binary_peers(buf, [])
  defp parse_binary_peers(buf, acc) when byte_size(buf) < 6, do: acc
  defp parse_binary_peers(buf, acc) do
     << ip0, ip1, ip2, ip3, port :: big-size(16), rest :: binary >> = buf
     peer = {Enum.join([ip0, ip1, ip2, ip3], "."), port}
     parse_binary_peers(rest, [peer | acc])
  end

  def handle_http_response(resp = %{"peers" => peers}) when is_list(peers) do
     peers = Enum.map(peers, fn p -> {p["id"], p["port"]} end)
     %{resp | "peers" => peers} |> to_response
  end

  def handle_http_response(resp = %{"peers" => peers}) when is_binary(peers) do
     %{resp | "peers" => parse_binary_peers(resp["peers"])} |> to_response
  end

  def handle_http_response(resp), do: to_response(resp)

  defp to_response(data) when is_map(data) do
    %Response{
      interval: data["interval"],
      num_seeders: data["complete"],
      num_leechers: data["incomplete"],
      tracker_id: data["tracker id"],
      failure_reason: data["failure reason"],
      peers: data["peers"],
    }
  end
end
