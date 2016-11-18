defmodule Peer.Connection do
  use Bitwise
  use GenServer

  defmodule State do
    defstruct sock: nil, in_stream: nil, out_stream: nil, buffer: <<>>
  end

  def start_link(sock) do
    :gen_fsm.start_link(__MODULE__, sock, [])
  end

  def init(sock, in_stream, out_stream) do
    {:ok, :wait_pstr, %State{sock: sock}}
  end

  def handle_info({:tcp, _sock, data}, state_name, state = %State{buffer: buf}) do
    :gen_fsm.send_event(self(), :consume)
    {:next_state, state_name, %{state | buffer: buf <> data}}
  end

  def handle_info(msg, state_name, state) do
    IO.puts(inspect msg)
    {:next_state, state_name, state}
  end
end
