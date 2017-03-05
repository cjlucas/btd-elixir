defmodule Peer.HandshakeUtils do
  @p 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563

  def gen_keys do
    privkey = :crypto.strong_rand_bytes(rand(160))
    pubkey = :crypto.mod_pow(2, privkey, @p)
    {privkey, pubkey}
  end

  def calc_secret(pubkey, privkey), do: :crypto.mod_pow(pubkey, privkey, @p)

  def init_streams(in_key, out_key) do
    [ins, outs] = [in_key, out_key]
                  |> Enum.map(&(:crypto.stream_init(:rc4, &1)))
                  |> Enum.map(&(:crypto.stream_decrypt(&1, :crypto.strong_rand_bytes(1024))))
                  |> Enum.map(&(elem(&1, 0)))
    {ins, outs}
  end

  def req1(s), do: hash([<<"req1">>, s])

  def req2(skey), do: hash([<<"req2">>, skey])

  def req3(s), do: hash([<<"req3">>, s])

  def key(prefix, s, skey), do: hash([prefix, s, skey])

  def hash(buf), do: :crypto.hash(:sha, buf)

  def rand(n), do: :rand.uniform(n+1) - 1

  def sync(needle, haystack), do: sync(needle, haystack, 0)
  defp sync(needle, haystack, _offset) when byte_size(needle) > byte_size(haystack), do: haystack
  defp sync(_needle, haystack, offset) when offset > byte_size(haystack), do: haystack
  defp sync(needle, haystack, offset) do
    size = byte_size(needle)
    case haystack do
      << _::bytes-size(offset), ^needle::bytes-size(size), rest::binary >> ->
        rest
      _ ->
        sync(needle, haystack, offset+1)
    end
  end

  def bin_xor(bin1, bin2) do
    use Bitwise

    [l1, l2] = Enum.map([bin1, bin2], &:erlang.binary_to_list/1)
    Enum.zip(l1, l2)
    |> Enum.map(fn e -> elem(e, 0) ^^^ elem(e, 1) end)
    |> :erlang.list_to_binary
  end
end
