defmodule HelperSigils do
  @lut "0123456789abcdef" |> String.codepoints

  defp lookup(c) do
    Enum.find_index(@lut, fn x -> x == c end)
  end

  def sigil_h(s, []) do
    s
    |> String.downcase
    |> String.codepoints
    |> Enum.map(&lookup/1)
    |> Enum.chunk(2)
    |> Enum.map(&(<< Enum.at(&1, 0)::4, Enum.at(&1, 1)::4 >>))
    |> :erlang.list_to_binary
  end
end

defmodule Doit do
  import HelperSigils

  def request do
		info_hash = ~h(0403fb4728bd788fbcb67e87d6feb241ef38c75a)
  	Tracker.Manager.register(info_hash, :crypto.strong_rand_bytes(20), ["http://torrent.ubuntu.com:6969/announce"])
		Tracker.Manager.request(info_hash)
  end
end

IO.puts inspect Process.whereis(Tracker.Manager)
for i <- 0..5, do: (IO.puts(i); Doit.request)

:timer.sleep(1000 * 60 * 60)
