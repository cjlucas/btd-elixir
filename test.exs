

defmodule Foo do
  def bar do
    buf = <<
    13 :: big-size(32),
    0x06,
    0x00, 0x00, 0x00, 0x01,
    0x00, 0x00, 0x00, 0x02,
    0x00, 0x00, 0x00, 0x03,
    >>

    IO.puts "here1"
    {:ok, conn} = :gen_tcp.connect({127, 0, 0, 1}, 4040, [:binary, active: false])
    IO.puts "here2"

    :ok = :gen_tcp.send(conn, buf)
    :ok = :gen_tcp.send(conn, << 1 :: big-size(32), 0 :: size(8)>>)
    :ok = :gen_tcp.send(conn, << 1 :: big-size(32), 1 :: size(8)>>)
    :ok = :gen_tcp.send(conn, << 1 :: big-size(32), 0 :: size(8)>>)
    :ok = :gen_tcp.send(conn, << 1 :: big-size(32), 1 :: size(8)>>)

    :gen_tcp.close(conn)
  end
end

0..1_000
|> Enum.map(fn _ -> Task.async(&Foo.bar/0) end)
|> Enum.map(fn task -> Task.await(task, :infinity) end)
