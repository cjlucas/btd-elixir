use Bitwise

p = 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563

skey = <<
0x94, 0x11, 0x2C, 0x62, 0x5A, 0x02, 0xBA, 0x80, 0x5A, 0xD0, 0xE0, 0x92, 0x87, 0xF4, 0xA7, 0xFC, 0xEF, 0xA8, 0x29, 0x1B
>>

vc = <<0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>>
crypto_provide = <<0x00, 0x00, 0x00, 0x03>>
ia = <<
  19,
  "BitTorrent protocol",
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
>> <> skey

len_ia = <<byte_size(ia) :: size(16)>>
IO.puts("LEN(ia) = #{inspect len_ia}")

bin_xor = fn bin1, bin2 ->
  {l1, l2} = {:erlang.binary_to_list(bin1), :erlang.binary_to_list(bin2)}
  Enum.zip(l1, l2)
  |> Enum.map(fn t -> elem(t, 0) ^^^ elem(t, 1) end)
  |> :erlang.list_to_binary
end


{:ok, listen} = :gen_tcp.listen(60451, [:binary, active: false, reuseaddr: true])
{:ok, conn} = :gen_tcp.accept(listen)

IO.puts(inspect conn)

xb = 2 <<< 158
yb = :crypto.mod_pow(2, xb, p)
IO.puts("Yb = #{inspect yb}")

:ok = :gen_tcp.send(conn, yb <> :crypto.strong_rand_bytes(100))

{:ok, ya} = :gen_tcp.recv(conn, 96)
IO.puts("Ya = #{inspect ya}")

{:ok, rest} = :gen_tcp.recv(conn, 0)
IO.puts("byte_size(rest) = #{byte_size(rest)}")

s = :crypto.mod_pow(ya, xb, p)
IO.puts("S = #{inspect s}")

req3 = :crypto.hash(:sha, <<?r, ?e, ?q, ?3>> <> s)
IO.puts("HASH('req3', S) = #{inspect req3}")

keya = :crypto.hash(:sha, <<?k, ?e, ?y, ?A>> <> s <> skey)
keyb = :crypto.hash(:sha, <<?k, ?e, ?y, ?B>> <> s <> skey)

IO.puts("keya = #{inspect keya}")
IO.puts("keyb = #{inspect keyb}")

{:ok, << req1 :: binary - size(20), xor :: binary - size(20), vc :: binary - size(8), _ :: binary>>} = :gen_tcp.recv(conn, 0)
IO.puts("XOR = #{inspect xor}")
IO.puts("req2 = #{inspect bin_xor.(xor, req3)}")

IO.puts("vc (enc) = #{inspect vc}")
streama = :crypto.stream_init(:rc4, keya)
{streama, _} = :crypto.stream_decrypt(streama, :crypto.strong_rand_bytes(1024))
{_, vc_dec} = :crypto.stream_decrypt(streama, vc)
IO.puts("vc (dec) = #{inspect vc_dec}")

:gen_tcp.close(listen)
