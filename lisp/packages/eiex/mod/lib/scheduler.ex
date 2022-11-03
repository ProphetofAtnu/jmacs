defmodule Eiex.Scheduler do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    {:ok, %{pending: []}}
  end

  def schedule(request) do
    GenServer.cast(__MODULE__, {:default, request})
  end

  @impl true
  def handle_cast({:default, request}, state) do
    Task.Supervisor.start_child(Eiex.Tasks, Eiex.Calls, :call, [request])
    {:noreply, state}
  end

end
