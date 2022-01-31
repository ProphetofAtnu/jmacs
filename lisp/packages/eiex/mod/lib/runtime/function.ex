defmodule Eiex.Runtime.Function do
  defdelegate info(fun), to: Function
  defdelegate info(fun, item), to: Function

  defp to_captured(fun) when is_function(fun), do: fun
  defp to_captured({mod, fun, ar}), do: Function.capture(mod, fun, ar)
end
