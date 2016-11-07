defmodule Peer.Connection do
  use Bitwise
  use GenServer

  @p 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563
  @vc <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>

  @pstr "BitTorrent protocol"

  @out_flow [
    :send_dh,
    :recv_dh,
    :calc_secret,
    :read_pad,
    :send_reqs,
    :recv_resp,
  ]

  defmodule State do
    defstruct sock: nil, buffer: <<>>
  end

  def start_link(:incoming, sock) do
    :gen_fsm.start_link(__MODULE__, {:in, sock}, [])
  end

  def start_link(:outgoing, opts = %OutOpts{}) do
    :gen_fsm.start_link(__MODULE__, {:out, nil}, [])
  end

  def init({:in, sock}) do
    {:ok, :wait_pstr, %State{sock: sock}}
  end

  def send_dh(:handle, state) do
    priv_key = :erlang.random((2 <<< 159) + 1) - 1
    pub_key = :crypto.mod_pow(2, priv_key, @p)
    padlen = :crypto.uniform(513) - 1
    pad = :crypto.strong_rand_bytes(padlen)

    :gen_tcp.send(state.sock, pub_key <> pad)

    {:next_state, :recv_dh, %{state | priv_key: priv_key}}
  end

  def recv_dh(:handle, state = %State{buffer: buf, priv_key: priv_key}) do
    case buf do
      << pub_key :: size(768) >> ->
        s = :crypto.mod_pow(pub_key, priv_key, @p)
        # TODO: need to check if incoming or outgoing to set proper key
    end
  end

  def handle_info({:tcp, _sock, data}, state_name, state = %State{buffer: buf}) do
    :gen_fsm.send_event(self(), :consume)
    {:next_state, state_name, %{state | buffer: buf <> data}}
  end

  def handle_info(msg, state_name, state) do
    IO.puts(inspect msg)
    {:next_state, state_name, state}
  end

  defp sha(buf) when is_binary(buf), do: :crypto.hash(:sha, buf)

  defp bin_xor(bin1, bin2) do
    [l1, l2] = Enum.map([bin1, bin2]) |> :erlang.binary_to_list
    Enum.zip(l1, l2)
    |> Enum.map(fn e -> elem(e, 0) ^^^ elem(e, 1) end)
    |> Enum.list_to_binary
  end
end
