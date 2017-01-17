defmodule Peer.Socket do
  defstruct in_stream: nil, out_stream: nil, sock: nil

  def send(%__MODULE__{out_stream: stream, sock: sock} = conn, data) when is_nil(stream) do
    case :gen_tcp.send(sock, data) do
      :ok -> {:ok, conn}
      {:error, reason} -> {:error, reason}
    end
  end

  def send(%__MODULE__{out_stream: stream, sock: sock} = conn, data) do
    {stream, data} = :crypto.stream_encrypt(stream, data)
    case :gen_tcp.send(sock, data) do
      :ok -> {:ok, %{conn | out_stream: stream}}
      {:error, reason} -> {:error, reason}
    end
  end

  def recv(%__MODULE__{sock: sock} = conn, len, timeout \\ 1000) do
    with {:ok, data} <- :gen_tcp.recv(sock, len, timeout) do
      {:ok, decrypt(conn, data)}
    end
  end

  def decrypt(%__MODULE__{in_stream: stream} = conn, data) when is_nil(stream) do
    {conn, data}
  end

  def decrypt(%__MODULE__{in_stream: stream} = conn, data) do
    {stream, data} = :crypto.stream_decrypt(stream, data)
    {%{conn | in_stream: stream}, data}
  end

  def close(%__MODULE__{sock: sock}) do
    :gen_tcp.close(sock)
  end
end
