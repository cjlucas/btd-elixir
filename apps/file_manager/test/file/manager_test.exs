defmodule File.ManagerTest do
  use ExUnit.Case

  @tag :skip
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
    File.mkdir("/tmp/btd")
    
    files = [{"1.mp3", 5}, {"2.mp3", 11}]
    {:ok, _} = File.Manager.start_link(<<>>, "/tmp/btd", files, [], 3)

    on_exit fn ->
      # kill all open filehandlers, this is necessary because File.Manager
      # will be sent a :shutdown signal, which won't trigger it's terminate callback
      Supervisor.which_children(Torrent.FileHandler.Supervisor)
      |> Enum.each(fn {_, pid, :worker, _} -> GenServer.stop(pid) end)

      File.rm_rf!("/tmp/btd")
    end

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
    
    test "write all pieces" do
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
