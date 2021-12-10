defmodule Eiex.Calls do

  @spec call(term(), term) :: {:ok, Eiex.IO.response(), term()} | {:error, String.t()}
  def call(:noop, state), do: {:ok, :noop, state}

  def call(:test, state), do: {:ok, {:write, "This is a test\n"}, state}

end
