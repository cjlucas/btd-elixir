defmodule BitSetTest do
  use ExUnit.Case

  test "setting each bit to 1" do
    for idx <- 0..15 do
      bs = BitSet.new(16)
      bs = BitSet.set(bs, idx, 1)
      assert BitSet.get(bs, idx) == 1

      for x <- 0..15, x != idx, do: assert BitSet.get(bs, x) == 0
    end
  end

  test "setting each bit to 0" do
      require Logger
    for idx <- 0..15 do
      bs = Enum.reduce(0..15, BitSet.new(16), fn x, acc -> BitSet.set(acc, x, 1) end)
      bs = BitSet.set(bs, idx, 0)
      assert BitSet.get(bs, idx) == 0

      for x <- 0..15, x != idx, do: assert BitSet.get(bs, x) == 1
    end
  end

  test "bad index" do
    bs = BitSet.new(16)
    assert_raise ArgumentError, fn ->
      BitSet.get(bs, 16)
    end
  end

  test "from binary" do
    bs = BitSet.from_binary(<<255, 255>>)
    for x <- 0..15, do: assert BitSet.get(bs, x) == 1
    
    bs = BitSet.from_binary(<<0::16>>)
    for x <- 0..15, do: assert BitSet.get(bs, x) == 0
  end
end
