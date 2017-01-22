defmodule File.ManagerTest do
  alias File.Manager.Store
  use ExUnit.Case

  setup_all do
    files = [{"1.mp3", 5}, {"2.mp3", 11}]
    pieces = [
      <<1, 2, 3>>,
      <<4, 5, 6>>,
      <<7, 8, 9>>,
      <<10, 11, 12>>,
      <<13, 14, 15>>,
      <<16>>
    ]

    hashes = Enum.map(pieces, &:crypto.hash(:sha, &1))
    :ok = Store.add(<<>>, "/tmp/btd", files, hashes, 3, 3)

    on_exit fn ->
      Store.remove(<<>>)
    end
  end

  setup do
    File.mkdir("/tmp/btd")

    {:ok, _} = File.Manager.start_link(<<>>)

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
      assert File.Manager.write_block(<<>>, 3, 0, <<10, 11, 12>>) == :ok
      assert File.read!("/tmp/btd/2.mp3") == <<0, 0, 0, 0, 10, 11, 12>>
    end
   
    test "block spanning two files" do
      assert File.Manager.write_block(<<>>, 1, 0, <<4, 5, 6>>) == :ok
      assert File.read!("/tmp/btd/1.mp3") == <<0, 0, 0, 4, 5>>
      assert File.read!("/tmp/btd/2.mp3") == <<6>>
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

    test "piece completion with good data" do
      assert File.Manager.write_block(<<>>, 0, 0, <<1, 2, 3>>) == :ok
      assert Store.blocks(<<>>, 0) |> Enum.map(&elem(&1, 2)) == [:verified]
    end

    test "piece completion with bad data" do
      assert File.Manager.write_block(<<>>, 0, 0, <<1, 2, 4>>) == {:error, :hash_check_failed}
      assert Store.blocks(<<>>, 0) |> Enum.map(&elem(&1, 2)) == [:need]
    end
    
    test "invalid permissions" do
      File.chmod("/tmp/btd", 0o400)
      assert File.Manager.write_block(<<>>, 0, 0, <<1, 2, 3>>) == {:error, :eacces}
    end
  end

  describe "read_block/4" do
    test "blocks are valid" do
      File.write!("/tmp/btd/1.mp3", <<1, 2, 3, 4, 5>>)
      File.write!("/tmp/btd/2.mp3", <<6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>)
      assert File.Manager.read_block(<<>>, 0, 0, 3) == {:ok, <<1, 2, 3>>}
      assert File.Manager.read_block(<<>>, 1, 0, 3) == {:ok, <<4, 5, 6>>}
      assert File.Manager.read_block(<<>>, 2, 0, 3) == {:ok, <<7, 8, 9>>}
      assert File.Manager.read_block(<<>>, 3, 0, 3) == {:ok, <<10, 11, 12>>}
      assert File.Manager.read_block(<<>>, 4, 0, 3) == {:ok, <<13, 14, 15>>}
      assert File.Manager.read_block(<<>>, 5, 0, 1) == {:ok, <<16>>}
    end

    test "missing block" do
      assert File.Manager.read_block(<<>>, 0, 0, 3) == {:error, :eof}
    end
  end
end
