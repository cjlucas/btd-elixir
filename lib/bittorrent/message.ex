defmodule Bittorrent.Message do
  defmodule Choke do
    defstruct id: 1
  end

  defmodule Request do
    defstruct index: 0, begin: 0, length: 0
  end

  defmodule Piece do
    defstruct index: 0, begin: 0, block: <<>>
  end

  @message_registry [
    {:choke, 1},
    {:request, 6},
    {:piece, 7}
  ]

  def parse(<< id, payload :: binary >>) do
    List.keyfind(@message_registry, id, 1)
    |> elem(0)
    |> decode(payload)
  end

  defp decode(:choke, _), do: {:ok, %Choke{}}

  defp decode(:request, payload) do
    case payload do
      <<
        index :: big-size(32),
        begin :: big-size(32), 
        len :: big-size(32)
      >> -> {:ok, %Request{index: index, begin: begin, length: len}}
      _ -> {:error, :decode_failed}
    end
  end

  defp decode(:piece, payload) do
    case payload do
      <<
        index :: big-size(32),
        begin :: big-size(32), 
        block :: binary
      >> -> {:ok, %Piece{index: index, begin: begin, block: block}}
      _ -> {:error, :decode_failed}
    end
  end

  defp decode(_, _payload) do
    {:error, :unknown_id}
  end
end
