defmodule File.ManagerTest do
  use ExUnit.Case

  test "segments" do
    import File.Manager, only: [segments: 2]
    files = [{"1.mp3", 5}, {"2.mp3", 10}]
    assert segments(%{offset: 3, size: 7}, files) == [
      {"1.mp3", 3, 2},
      {"2.mp3", 0, 5}
    ]
    
    files = [{"1.mp3", 5}, {"2.mp3", 10}]
    assert segments(%{offset: 0, size: 15}, files) == [
      {"1.mp3", 0, 5},
      {"2.mp3", 0, 10}
    ]
  end

  test "start_link" do
    files = [{"1.mp3", 5}, {"2.mp3", 10}]
    {:ok, pid} = File.Manager.start_link(<<>>, "/tmp/wtf", files, [], 3)
    assert is_pid(pid)

    :ok = File.Manager.write_block(<<>>, 0, 0, <<1, 2, 3>>)
  end
end
