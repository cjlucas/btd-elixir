defmodule Bittorrent.Message do
  defmodule Choke do
    defstruct id: 1
  end

  defmodule Request do
    defstruct id: 6, index: 0, begin: 0, length: 0
  end

  defmodule Piece do
    defstruct id: 7, index: 0, begin: 0, block: <<>>
  end

  def parse(<< id, payload :: binary >>) do
    case id do
      1 -> {:ok, %Choke{}}
      6 -> decode(%Request{}, payload)
      7 -> decode(%Piece{}, payload)
      _ -> {:error, :unknown_id}
    end
  end

  defp decode(msg = %Request{}, payload) do
    case payload do
      <<
        index :: big-size(32),
        begin :: big-size(32), 
        len :: big-size(32)
      >> -> {:ok, %{msg | index: index, begin: begin, length: len}}
      _ -> {:error, :decode_failed}
    end
  end

  defp decode(msg = %Piece{}, payload) do
    case payload do
      <<
        index :: big-size(32),
        begin :: big-size(32), 
        block :: binary
      >> -> {:ok, %{msg | index: index, begin: begin, block: block}}
      _ -> {:error, :decode_failed}
    end
  end
end
