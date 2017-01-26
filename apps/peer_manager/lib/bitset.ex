defmodule BitSet do
  defstruct bits: nil

  use Bitwise

  def new(size) when is_number(size) and rem(size, 8) == 0 do
    %BitSet{bits: :array.new(size: div(size, 8), default: 0)}
  end
  def new(size) when is_number(size) do
    new(size+1)
  end

  def get(%{bits: bits}, index) do
    arr_index = div(index, 8)
    bit = rem(index, 8)
    (:array.get(arr_index, bits) >>> (7 - bit)) &&& 0b1
  end

  def set(%{bits: bits}, index, value) when value in [0, 1] do
    arr_index = div(index, 8)
    bit = rem(index, 8)
    shift_amt = 7-bit

    byte = :array.get(arr_index, bits)
    byte =
      if value == 0 do
        byte &&& (0xFF ^^^ (1 <<< shift_amt))
      else
        byte ||| (1 <<< shift_amt)
      end

    %BitSet{bits: :array.set(arr_index, byte, bits)}
  end

  def from_binary(binary), do: from_binary(binary, 0, BitSet.new(8 * byte_size(binary)))
  def from_binary(binary, idx, acc) when idx >= 8 * byte_size(binary), do: acc
  def from_binary(binary, idx, %{bits: bits}) do
    byte_offset = div(idx, 8)
    <<_::bytes-size(byte_offset), byte::8, _::binary>> = binary
    from_binary(binary, idx+1, %BitSet{bits: :array.set(byte_offset, byte, bits)})
  end
end
