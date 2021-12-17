defmodule Eiex.Runtime do
  use GenServer

  @type exp :: {String.t(), list(integer())}
  @type mod :: %{atom() => exp()}

  defdelegate exports(mod), to: IEx.Autocomplete

  defp format_exports(mod) do
    exports(mod)
    |> Enum.group_by(&(elem(&1, 0)), &(elem(&1, 1)))
    |> Enum.map(fn {k, v} -> {k, {Atom.to_string(k), v}} end)
  end
  

  def collect_runtime() do
    :code.module_status()
    |> Enum.map(fn {mod, :loaded} ->
      {mod, format_exports(mod)}
    end) |> Enum.into(%{})
  end

  def start_link(_opt) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)

  end

  @impl true
  def init(_init_arg) do
    runtime = collect_runtime()
    {:ok, %{runtime: runtime}}
  end

  def clean_alias(arg) do
    try do 
      case arg do
        "" -> Kernel
        <<?:, rest::binary>> -> String.to_existing_atom(rest)
        <<"Elixir.", _::binary>> -> String.to_existing_atom(arg)
        _ -> String.to_existing_atom("Elixir." <> arg)
      end
    catch
      _, _ -> :invalid
    end
  end

  def to_qualified(arg) when is_binary(arg) do
    {prt, rest} = String.split(arg, ".") |> List.pop_at(-1)
    mod = Enum.join(rest, ".") |> clean_alias()
    {mod, prt}
  end

  def find(mod, part, state) do
    with {:ok, exp} <- Map.fetch(state[:runtime], mod) do
      Enum.filter(exp, fn {_, {sn, _}} ->
        String.starts_with?(sn, part)
      end)
    else
      :error -> []
    end
  end

  def complete(arg) do
    GenServer.call(__MODULE__, {:complete, arg})
  end

  @impl true
  def handle_call({:complete, part}, _from, state) do
    case part do
      {mod, part} ->
        {:reply, find(mod, part, state), state}
      p when is_binary(p) ->
        {mod, part} = to_qualified(p)
        {:reply, find(mod, part, state), state}
    end
  end

  @impl true
  def handle_cast(:update, state) do
    {:noreply, %{state | runtime: collect_runtime()}}
  end
  
end
