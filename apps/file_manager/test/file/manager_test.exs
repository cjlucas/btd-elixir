defmodule File.ManagerTest do
  use ExUnit.Case, async: false

  test "segments" do
    import File.Manager, only: [segments: 3]
    files = [{"1.mp3", 5}, {"2.mp3", 10}]
    assert segments(3, 7, files) == [
      {"1.mp3", 3, 2},
      {"2.mp3", 0, 5}
    ]
    
    files = [{"1.mp3", 5}, {"2.mp3", 10}]
    assert segments(0, 15, files) == [
      {"1.mp3", 0, 5},
      {"2.mp3", 0, 10}
    ]
    
    files = [{"1.mp3", 5}, {"2.mp3", 11}]
    assert segments(15, 1, files) == [
      {"2.mp3", 10, 1}
    ]
  end

  setup do
    IO.puts("OMGHERE")
    File.rm_rf!("/tmp/btd")
    File.mkdir("/tmp/btd")
    
    files = [{"1.mp3", 5}, {"2.mp3", 11}]
    {:ok, pid} = File.Manager.start_link(<<>>, "/tmp/btd", files, [], 3)

    :ok
  end

  describe "write_block/4" do
    test "block at the beginning of a file" do
      assert File.Manager.write_block(<<>>, 0, 0, <<1, 2, 3>>) == :ok
      assert File.read!("/tmp/btd/1.mp3") == <<1, 2, 3>>
    end
  
    test "block in the middle of a file" do
      assert File.Manager.write_block(<<>>, 3, 0, <<1, 2, 3>>) == :ok
      assert File.read!("/tmp/btd/2.mp3") == <<0, 0, 0, 0, 1, 2, 3>>
    end
   
    test "block spanning two files" do
      assert File.Manager.write_block(<<>>, 1, 0, <<1, 2, 3>>) == :ok
      assert File.read!("/tmp/btd/1.mp3") == <<0, 0, 0, 1, 2>>
      assert File.read!("/tmp/btd/2.mp3") == <<3>>
    end
    
    test "write entire fileset" do
      assert File.Manager.write_block(<<>>, 0, 0, <<1, 2, 3>>) == :ok
      assert File.Manager.write_block(<<>>, 1, 0, <<4, 5, 6>>) == :ok
      assert File.Manager.write_block(<<>>, 2, 0, <<7, 8, 9>>) == :ok
      assert File.Manager.write_block(<<>>, 3, 0, <<10, 11, 12>>) == :ok
      assert File.Manager.write_block(<<>>, 4, 0, <<13, 14, 15>>) == :ok
      assert File.Manager.write_block(<<>>, 5, 0, <<16>>) == :ok
      assert File.read!("/tmp/btd/1.mp3") == <<1, 2, 3, 4, 5>>
      assert File.read!("/tmp/btd/2.mp3") == <<6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>
    end
  end
end
