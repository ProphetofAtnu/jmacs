defmodule Eiex.Server do
  use GenServer

  import GenServer 


  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(init_arg) do
    port = Keyword.get(init_arg, :port, 48771)
    {:ok, listen_sock} = :gen_tcp.listen(port,
      [:binary, reuseaddr: true, active: true])
    cast(self(), :accept)
    {:ok, %{port: port, listen_sock: listen_sock, sock: nil}}
  end

  def client() do
    call(__MODULE__, :client)
  end

  def send(term) do
    call(__MODULE__, {:send, term})
  end

  @impl true
  def handle_call(request, _from, state) do
    case request do
      :client -> {:reply, state[:sock], state}
      {:send, term} ->
        with {:ok, sock} <- Map.fetch(state, :sock) do
          {:reply, :gen_tcp.send(sock, :erlang.term_to_binary(term)), state}
        else
          _ -> {:reply, {:error, :nosock}, state}
        end
      _ -> {:reply, nil, state}
    end
  end

  @impl true
  def handle_cast(:accept, state) do
    case :gen_tcp.accept(state[:listen_sock]) do
      {:ok, sock} -> {:noreply, %{state | sock: sock}}
      {:error, _} ->
        cast(self(), :accept)
        {:ok, state}
    end
  end

  @impl true
  def handle_info(tcpm, state) do
    case tcpm do
      {:tcp, _socket, packet} ->
        IO.inspect(packet)
        {:noreply, state}
      {:tcp_closed, _socket} ->
        IO.puts("Socket disconnected")
        cast(self(), :accept)
        {:noreply, state}
      {:tcp_error, socket, reason} ->
        :gen_tcp.close(socket)
        IO.puts("Closed socket: #{reason}")
        cast(self(), :accept)
        {:noreply, state}
      x ->
        IO.puts("Unknown info received #{x}")
        {:noreply, state}
    end 
  end
end
