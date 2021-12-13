defmodule Eiex.IO do
  use GenServer

  @type response ::
          :noop
          | {:write, iodata()}
          | {:error, String.t()}

  def start_link(_opts \\ []) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def convert_binary(bin) do
    try do
      {:ok, :erlang.binary_to_term(bin)}
    catch 
    _, r -> {:error, r}
    end
  end

  defp do_read(data) do
    GenServer.cast(__MODULE__, {:message, data})
  end

  defp do_write(term) do
    :erlang.term_to_binary(term)
    |> Base.encode64()
    |> IO.puts()
  end

  def send(term) do
    do_write(term)
  end


  def loop_input() do 
    line = IO.read(:stdio, :line)
    with {:ok, data} <- Base.decode64(line, [ignore: :whitespace]),
         {:ok, term} = convert_binary(data) do
      do_read(term)
    end
    loop_input()
  end

  @impl true
  def init(_opts) do
    start_input_loop()
    {:ok, %{lisp_opts: [:list_tuple]}}
  end

  defp start_input_loop() do
    {pid, _} = spawn_monitor(__MODULE__, :loop_input, [])
    Process.put(:input_loop, pid)
  end

  defp handle_message(data, state) do
    Eiex.Calls.call(data, state)

    state
  end

  @impl true
  def handle_cast({:message, data}, state) do
    ns = handle_message(data, state)
    {:noreply, ns}
  end

  @impl true
  def handle_cast({:write, data}, state) do
    do_write(data)
    {:noreply, state}
  end


  @impl true
  def handle_info({:DOWN, _, _, _, reason}, state) do
    case reason do
      :normal ->
        {:stop, :normal, state}

      _ ->
        start_input_loop()
        {:noreply, state}
    end
  end

  @impl true
  def handle_call(request, _from, state) do
    IO.inspect(request)
    {:reply, nil, state}
  end
end
