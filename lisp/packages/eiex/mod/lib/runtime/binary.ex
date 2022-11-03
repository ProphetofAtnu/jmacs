defmodule Eiex.Runtime.Binary do
  
  def to_qualified(arg) when is_binary(arg) do
    {prt, rest} = String.split(arg, ".") |> List.pop_at(-1)
    mod = Enum.join(rest, ".") 
    {mod, prt}
  end

  def qualified?(arg) do
    String.contains?(arg, ".")
  end
end
