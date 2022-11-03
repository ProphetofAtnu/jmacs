defmodule Eiex.Runtime do
  use GenServer
  import Eiex.Runtime.Introspect

  def start_link(_opt) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)

  end

  @impl true
  def init(_init_arg) do
    runtime = collect_runtime()
    {:ok, %{runtime: runtime}}
  end

  def complete(arg) do
    GenServer.call(__MODULE__, {:complete, arg})
    |> Enum.map(
    fn
      {mod, _, s, arity} ->
        [s, :fun, arity, mod]
      {_, s} ->
        [s, :mod]
    end)
  end

  def update(), do: GenServer.cast(__MODULE__, :update)

  def timestamp(), do:
  GenServer.call(__MODULE__, :stamp)
  |> Time.to_string()

  @impl true
  def handle_call({:complete, part}, _from, state) do
    {:reply, find(state[:runtime], part), state}
  end

  @impl true
  def handle_call(:stamp, _from, state) do
    {:reply, state[:runtime].last_update, state}
  end

  @impl true
  def handle_cast(:update, state) do
    {:noreply, %{state | runtime: collect_runtime()}}
  end
  
end
