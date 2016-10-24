defmodule Bittorrent.FileStream do
  defmodule Item do
    defstruct name: nil, offset: 0, size: 0
  end

  def items_in_range(items, offset, length) do
    items = Enum.filter(items, &(offset >= &1.offset || offset+length >= &1.offset))
    items_in_range(items, offset, length, [])
  end

  defp items_in_range(_, _, length, acc) when length == 0, do: Enum.reverse(acc)
  defp items_in_range([item | rest], offset, length, acc) do
    n = Enum.min([length, item.size - offset])
    section = %Item{name: item.name, offset: offset, size: n}
    items_in_range(rest, 0, length-n, [section | acc])
  end
end
