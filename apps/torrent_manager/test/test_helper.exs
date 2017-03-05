ExUnit.configure(capture_log: true)
ExUnit.start()

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
