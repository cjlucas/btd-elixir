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


{:ok, conn} = :gen_tcp.connect({192, 168, 1, 11}, 64452, [:binary, active: false])
IO.puts(inspect conn)

xa = 2 <<< 159
ya = :crypto.mod_pow(2, xa, p)
IO.puts("Ya = #{inspect ya}")

:ok = :gen_tcp.send(conn, ya <> :crypto.strong_rand_bytes(10))

{:ok, yb} = :gen_tcp.recv(conn, 96)
IO.puts("Yb = #{inspect yb}")

{:ok, rest} = :gen_tcp.recv(conn, 0)
IO.puts("byte_size(rest) = #{byte_size(rest)}")

s = :crypto.mod_pow(yb, xa, p)
IO.puts("S = #{inspect s}")

keya = :crypto.hash(:sha, <<"keyA">> <> s <> skey)
keyb = :crypto.hash(:sha, <<"keyB">> <> s <> skey)
IO.puts("keya = #{inspect keya}")
IO.puts("keyb = #{inspect keyb}")

req1 = :crypto.hash(:sha, <<"req1">> <> s)
IO.puts("HASH('req1', S) = #{inspect req1}")
req2 = :crypto.hash(:sha, <<"req2">> <> skey)
IO.puts("HASH('req2', SKEY) = #{inspect req2}")
req3 = :crypto.hash(:sha, <<"req3">> <> s)
IO.puts("HASH('req3', S) = #{inspect req3}")

streama = :crypto.stream_init(:rc4, keya)

omg = vc <> crypto_provide <> <<0x00, 0x00>> <> len_ia
IO.puts("HI #{inspect omg}")
{streama, _} = :crypto.stream_encrypt(streama, :crypto.strong_rand_bytes(1024))
{streama, enc} = :crypto.stream_encrypt(streama, omg)
{streama, enc2} = :crypto.stream_encrypt(streama, ia)

IO.puts("OMGHERE #{byte_size(enc)}")
IO.puts("WEEE #{inspect bin_xor.(req2, req3)}")
payload = req1 <> bin_xor.(req2, req3) <> enc <> enc2
:gen_tcp.send(conn, payload)

{:ok, resp_enc} = :gen_tcp.recv(conn, 0)
IO.puts(inspect resp_enc)
streamb = :crypto.stream_init(:rc4, keyb)
{streamb, _} = :crypto.stream_encrypt(streamb, :crypto.strong_rand_bytes(1024))
{streamb, resp} = :crypto.stream_decrypt(streamb, resp_enc)
IO.puts(inspect resp)


:gen_tcp.close(conn)
