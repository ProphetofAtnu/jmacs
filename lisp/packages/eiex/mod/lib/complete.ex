defmodule Eiex.Complete do
  alias Eiex.Buffer

  def complete({:part, data}) do
    Buffer.suggest(data)
  end
  
  def complete({:at, data, pos}) do
    Buffer.suggest(data, pos)
  end
  
end
