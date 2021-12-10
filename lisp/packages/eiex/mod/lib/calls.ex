defmodule Eiex.Calls do

  def format(arg), do: inspect(arg, limit: :infinity)

  @spec call(term(), term) :: {:ok, Eiex.IO.response(), term()} | {:error, String.t()}
  def call(:noop, state), do: {:ok, :noop, state}

  def call(:test, state), do: {:ok, {:write, "This is a test\n"}, state}

  def call({:complete, args}, state), do:
  {:ok, {:write, format(Eiex.Complete.complete(args))}, state}

end
