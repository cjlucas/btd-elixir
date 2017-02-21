defmodule BitSet do
  defstruct [:size, :bits]

  use Bitwise

  def new(size) when is_number(size) and rem(size, 8) == 0 do
    %BitSet{size: size, bits: :array.new(size: div(size, 8), default: 0)}
  end
  def new(size) when is_number(size) do
    %BitSet{size: size, bits: :array.new(size: div(size, 8)+1, default: 0)}
  end

  def get(%__MODULE__{bits: bits}, index) do
    arr_index = div(index, 8)
    bit = rem(index, 8)
    (:array.get(arr_index, bits) >>> (7 - bit)) &&& 0b1
  end

  def set(%__MODULE__{bits: bits} = bitset, index, value) when value in [0, 1] do
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

    %{bitset | bits: :array.set(arr_index, byte, bits)}
  end

  def from_binary(binary), do: from_binary(byte_size(binary) * 8, binary)
  def from_binary(size, binary), do: from_binary(binary, 0, BitSet.new(size))
  defp from_binary(_binary, byte_offset, %__MODULE__{size: size} = acc) when byte_offset >= div(size, 8), do: acc
  defp from_binary(binary, byte_offset, %__MODULE__{bits: bits} = bitset) do
    <<_::bytes-size(byte_offset), byte::8, _::binary>> = binary
    from_binary(binary, byte_offset+1, %{bitset | bits: :array.set(byte_offset, byte, bits)})
  end
end

defimpl Enumerable, for: BitSet do
  def count(%BitSet{size: size}), do: {:ok, size}

  def member?(_bitset, el) when el < 0 or el > 1, do: {:ok, false}
  def member?(%BitSet{size: size} = bitset, el) do
    {:ok, Enum.any?(0..size-1, &BitSet.get(bitset, &1) == el)}
  end

	def reduce(%BitSet{size: size} = bitset, acc, fun) do
    l = for i <- 0..size-1, do: BitSet.get(bitset, i)
    Enumerable.List.reduce(l, acc, fun)
	end
end
