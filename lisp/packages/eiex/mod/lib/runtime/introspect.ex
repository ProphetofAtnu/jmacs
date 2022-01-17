defmodule Eiex.Runtime.Introspect do
  import Eiex.Runtime.Binary

  defdelegate exports(mod), to: IEx.Autocomplete

  defstruct modules: [],
            exported: %{},
            last_update: nil

  @type exp :: {atom(), atom(), String.t(), list(integer())}

  def collect_exports(mod) do
    exports(mod)
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.map(fn {k, v} -> {mod, k, Atom.to_string(k), v} end)
  end

  def collect_modules() do
    :code.module_status()
    |> Enum.map(&elem(&1, 0))
    |> Enum.map(&{&1, inspect(&1)})
  end

  def collect_runtime() do
    mods = collect_modules()
    exps =
      mods
      |> Enum.map(fn {mod, _} ->
        {mod, collect_exports(mod)}
      end)
      |> Enum.into(%{})

    %__MODULE__{modules: mods, exported: exps, last_update: Time.utc_now()}
  end


  def find_exported(rt, mod, prefix) when is_nil(mod) do
    find_exported(rt, Kernel.SpecialForms, prefix) ++
      find_exported(rt, Kernel, prefix)
  end

  def find_exported(rt, mod, prefix) when is_atom(mod) do
    Map.get(rt.exported, mod, [])
    |> Enum.filter(fn {_, _, str, _} ->
      String.starts_with?(str, prefix)
    end)
  end

  def find_exported(rt, mod, prefix) when is_binary(mod) do
    with {matm, _} <- Enum.find(rt.modules, :none, fn {_, str} -> str == mod end) do
      find_exported(rt, matm, prefix)
    else
      _ -> []
    end
  end

  def find_module(rt, mod) do
    rt.modules
    |> Enum.filter(fn {_, m} ->
      String.starts_with?(m, mod)
    end)
  end

  def find(rt, expr) do
    {mod, fun} = to_qualified(expr)
    mods = find_module(rt, expr)
    |> Enum.map(fn {atm, str} ->
      {atm, String.replace_leading(str, mod<>".", "")}
    end)
    amod = if mod == "", do: nil, else: mod
    mods ++ find_exported(rt, amod, fun)
  end
end
