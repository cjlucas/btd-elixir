defmodule Peer.Message do
  defmodule Choke do
    defstruct []
  end

  defmodule Unchoke do
    defstruct []
  end

  defmodule Interested do
    defstruct []
  end

  defmodule NotInterested do
    defstruct []
  end

  defmodule Have do
    defstruct index: 0
  end

  defmodule Bitfield do
    defstruct bitfield: <<>>
  end

  defmodule Request do
    defstruct index: 0, begin: 0, length: 0
  end

  defmodule Piece do
    defstruct index: 0, begin: 0, block: <<>>
  end

  defmodule Cancel do
    defstruct index: 0, begin: 0, length: 0
  end

  defmodule Port do
    defstruct port: 0
  end

  @message_registry [
    {:choke, 0},
    {:unchoke, 1},
    {:interested, 2},
    {:not_interested, 3},
    {:have, 4},
    {:bitfield, 5},
    {:request, 6},
    {:piece, 7},
    {:cancel, 8},
    {:port, 9},
  ]

  def parse(<<id, _::binary>>) when id > 9, do: {:error, :unknown_id}
  def parse(<< id, payload :: binary >>) do
    List.keyfind(@message_registry, id, 1)
    |> elem(0)
    |> decode(payload)
  end

  defp decode(:choke, _), do: {:ok, %Choke{}}
  defp decode(:unchoke, _), do: {:ok, %Unchoke{}}
  defp decode(:interested, _), do: {:ok, %Interested{}}
  defp decode(:not_interested, _), do: {:ok, %NotInterested{}}
  defp decode(:bitfield, payload), do: {:ok, %Bitfield{bitfield: payload}}

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

  defp decode(:have, payload) do
    case payload do
      << index :: big-size(32) >> -> {:ok, %Have{index: index}}
      _ -> {:error, :decode_failed}
    end
  end

  defp decode(:cancel, payload) do
    case payload do
      <<
        index :: big-size(32),
        begin :: big-size(32),
        length :: big-size(32),
      >> -> {:ok, %Cancel{index: index, begin: begin, length: length}}
      _ -> {:error, :decode_failed}
    end
  end

  defp decode(:port, payload) do
    case payload do
      << port :: big-size(32) >> -> {:ok, %Port{port: port}}
      _ -> {:error, :decode_failed}
    end
  end

  defp decode(_, _payload) do
    {:error, :unknown_id}
  end

  defp msg_id(type) do
    List.keyfind(@message_registry, type, 0)
    |> elem(1)
  end

  def encode(%Choke{}), do: << msg_id(:choke) :: size(8) >>
  def encode(%Unchoke{}), do: << msg_id(:unchoke) :: size(8) >>
  def encode(%Interested{}), do: << msg_id(:interested) :: size(8) >>
  def encode(%NotInterested{}), do: << msg_id(:not_interested) :: size(8) >>

  def encode(%Have{index: index}) do
    << msg_id(:have) :: size(8), index :: big-size(32) >>
  end

  def encode(%Bitfield{bitfield: bitfield}) do
    << msg_id(:bitfield) :: size(8), bitfield :: binary >>
  end

  def encode(%Request{index: index, begin: begin, length: length}) do
    <<
      msg_id(:request) :: size(8),
      index :: big-size(32),
      begin :: big-size(32),
      length :: big-size(32),
    >>
  end

  def encode(%Piece{index: index, begin: begin, block: block}) do
    <<
      msg_id(:piece) :: size(8),
      index :: big-size(32),
      begin :: big-size(32),
      block :: binary,
    >>
  end

  def encode(%Cancel{index: index, begin: begin, length: length}) do
    <<
      msg_id(:cancel) :: size(8),
      index :: big-size(32),
      begin :: big-size(32),
      length :: big-size(32),
    >>
  end

  def encode(%Port{port: port}) do
    <<
      msg_id(:port) :: size(8),
      port :: big-size(32),
    >>
  end
end
