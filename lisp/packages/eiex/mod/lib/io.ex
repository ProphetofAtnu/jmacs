defmodule Eiex.IO do
  use GenServer

  def start_link(_opts \\ []) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def loop_input(state \\ []) do
    case IO.read(1) do
      <<7>> ->
        case GenServer.call(__MODULE__,
              {:message, Enum.reverse(state) |> IO.iodata_to_binary}) do
          {:write, data} -> IO.puts(data)
          _ -> :noop
        end
        loop_input()
      :eof -> exit(:normal)
      c -> loop_input([c | state])
    end
  end
  

  @impl true
  def init(_opts) do
    start_input_loop()
    {:ok, %{}}
  end

  defp start_input_loop() do
    {pid, _} = spawn_monitor(__MODULE__, :loop_input, [])
    Process.put(:input_loop, pid)
  end

  defp handle_message(data, state) do
    with {:ok, expr} <- Eiex.Lisp.translate(data) do
      {{:write, expr}, state}
    else
      e -> {{:write, inspect(e)}}
    end
  end

  @impl true
  def handle_call({:message, data}, _from, state) do
    {r, ns} = handle_message(data, state)
    {:reply, r, ns}
  end

  @impl true 
  def handle_info({:DOWN, _, _, _, reason}, state) do
    IO.puts("Process exited")
    case reason do
      :normal -> {:stop, :normal, state}
      _ -> start_input_loop()
        {:noreply, state}

    end
  end

end
