defmodule ConnProfile do
  import ExProf.Macro

  @data :crypto.strong_rand_bytes(16834)
  @msg %Bittorrent.Message.Piece{block: @data}
  @payload :erlang.iolist_to_binary([<<16843::32>>, Bittorrent.Message.encode(@msg)])

  def blah do
    state = %Peer.Connection.State{sock: %Peer.Socket{}}

    for _ <- 1..400_000 do
      {noreply, state} = Peer.Connection.handle_info({:tcp, nil, @payload}, state)
    end
  end

  @raw :crypto.strong_rand_bytes(1400)
  @sz 100_000_000

  def blah2(acc) do
    if :erlang.iolist_size(acc) > @sz do
      acc
    else
      blah2([acc, @raw])
    end
  end

  def blah3(acc, total) do
    if total > @sz do
      acc
    else
      blah3([acc, @raw], total + 1400)
    end
  end

  def blah4(acc) do
    if byte_size(acc) > @sz do
      acc
    else
      blah4(acc <> @raw)
    end
  end

  def run do
    #profile do
      #blah
    #end

    profile do
      data = blah2([]) |> :erlang.iolist_to_binary
      IO.puts(byte_size(data))
    end

    profile do
      data = blah3([], 0) |> :erlang.iolist_to_binary
      IO.puts(byte_size(data))
    end
    profile do
      data = blah4(<<>>)
      IO.puts(byte_size(data))
    end
  end
end

ConnProfile.run
