

defmodule Foo do
  def bar do
    #buf = <<
    #13 :: big-size(32),
    #0x06,
    #0x00, 0x00, 0x00, 0x01,
    #0x00, 0x00, 0x00, 0x02,
    #0x00, 0x00, 0x00, 0x03,
    #>>

    IO.puts "OMGHERE"
    {:ok, conn} = :gen_tcp.connect({127, 0, 0, 1}, 4040, [:binary, active: false])

    #for n <- 0..100_000, do: :ok = :gen_tcp.send(conn, buf)
    :ok = :gen_tcp.send(conn, << 1 :: big-size(32), 0 :: size(8)>>)
    :ok = :gen_tcp.send(conn, << 1 :: big-size(32), 1 :: size(8)>>)
    :ok = :gen_tcp.send(conn, << 1 :: big-size(32), 0 :: size(8)>>)
    :ok = :gen_tcp.send(conn, << 1 :: big-size(32), 1 :: size(8)>>)
  end
end


Foo.bar

for _ <- 0..100, do: spawn fn -> Foo.bar end
