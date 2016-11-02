defmodule Peer.Connection do
  @behaviour :gen_fsm

  defmodule State do
    defstruct sock: nil, buffer: <<>>
  end

  def start_link({:in, sock}) do
    :gen_fsm.start_link(__MODULE__, {:in, sock}, [])
  end

  def start_link({:out, addr, port}) do
    :gen_fsm.start_link(__MODULE__, {:out, addr, port}, [])
  end

  def init({:in, sock}) do
    {:ok, :wait_pstr, %State{sock: sock}}
  end

  def init({:out, addr, port}) do
    case :gen_tcp.connect(addr, port, [:binary, active: false], 5000) do
      {:ok, sock} ->
        :gen_tcp.controlling_process(sock, self())
        {:ok, :wait_pstr, %State{sock: sock}} # TODO: not the right state
      {:error, reason} ->
        {:stop, reason}
    end
  end

  def wait_pstr(:consume, state) do
    IO.puts("IN wait_pstr #{inspect state.buffer}")
    {:next_state, :wait_pstr, state}
  end

  def handle_info({:tcp, _sock, data}, state_name, state = %State{buffer: buf}) do
    :gen_fsm.send_event(self(), :consume)
    {:next_state, state_name, %{state | buffer: buf <> data}}
  end
end
