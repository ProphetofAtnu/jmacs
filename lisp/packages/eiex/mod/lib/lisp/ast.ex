defmodule Eiex.Lisp.AST do
  defstruct level: 0, children: [], marker: :root

  @type t :: %__MODULE__{}

  defguard is_node(obj) when :erlang.map_get(:__struct__, obj) == __MODULE__

  @spec walk(t(), term(), (t(), term() -> term())) :: term()
  def walk(_, _, _)

  def walk(n, acc, red) when is_node(n) do
    acc = red.(n, acc)

    children = n.children

    Enum.reduce(children, acc, fn c, acc ->
      walk(c, acc, red)
    end)
  end

  def walk(n, acc, red) do
    red.(n, acc)
  end

  def interpret(n, opts \\ [])

  def interpret(n, _opts) when is_binary(n) do
    cond do
      is_string?(n) -> interpret_string(n)
      is_int?(n) -> interpret_int(n)
      is_float?(n) -> interpret_float(n)
      is_keyword?(n) -> interpret_keyword(n)
      true -> String.to_atom(n)
    end
  end

  def interpret(obj, opts) when is_node(obj) do
    cond do
      is_root?(obj) -> interpret(hd(obj.children), opts)
      is_vector?(obj) -> interpret_vector(obj, opts)
      is_alist?(obj) -> interpret_alist(obj, opts)
      is_pair?(obj) -> interpret_pair(obj, opts)
      !(:no_plists in opts) && is_plist?(obj)  -> interpret_plist(obj, opts)
      true -> interpret_list(obj, opts)
    end
  end

  @spec is_node?(term()) :: boolean()
  def is_node?(obj) do
    is_map(obj) && Map.get(obj, :__struct__) == __MODULE__
  end

  @spec is_root?(term()) :: boolean()
  def is_root?(obj) do
    is_node?(obj) && obj.level == 0
  end

  defp is_int?(obj) do
    match?({_, ""}, Integer.parse(obj))
  end

  defp interpret_int(n) do
    {x, ""} = Integer.parse(n)
    x
  end

  defp is_float?(obj) do
    Float.parse(obj) != :error
  end

  defp interpret_float(n) do
    {x, ""} = Float.parse(n)
    x
  end

  defp is_keyword?(obj) do
    is_binary(obj) && String.starts_with?(obj, ":")
  end

  defp interpret_keyword(obj) do
    String.trim_leading(obj, ":") |> String.to_atom()
  end
    

  defp is_string?(obj) do
    is_binary(obj) && String.starts_with?(obj, "\"") && String.ends_with?(obj, "\"")
  end

  defp interpret_string(n), do: String.trim(n, "\"")

  defp is_list?(obj) do
    obj.marker == "("
  end

  defp is_vector?(obj) do
    obj.marker == "["
  end

  def interpret_vector(ast, opts) do
    vec = Enum.map(ast.children, &interpret(&1, opts))

    if :vec_list in opts do
      vec
    else
      List.to_tuple(vec)
    end
  end

  defp is_pair?(ast) do
    is_list?(ast) && match?([_, ".", _], ast.children)
  end

  defp interpret_pair(ast, opts) do
    [l, ".", r] = ast.children
    {interpret(l, opts), interpret(r, opts)}
  end

  defp is_alist?(obj) do
    is_list?(obj) &&
      obj.children
      |> Enum.all?(fn node ->
        is_node?(node) &&
          Enum.all?(node.children, &(!is_node?(&1))) &&
          match?([_, ".", _], node.children)
      end)
  end

  defp interpret_alist(ast, opts) do
    for c <- ast.children,
        is_node?(c),
        [k, ".", v | _] = c.children,
        into: %{} do
      {interpret(k, opts), interpret(v, opts)}
    end
  end

  defp is_plist?(ast) do
    is_list?(ast) &&
      Enum.chunk_every(ast.children, 2)
      |> Enum.map(&List.to_tuple/1)
      |> Enum.all?(fn {x, _} ->
        !is_node?(x) && is_keyword?(x)
      end)
  end

  defp interpret_plist(ast, opts) do
    children = ast.children

    Enum.chunk_every(children, 2)
    |> Enum.map(&List.to_tuple/1)
    |> Enum.map(fn {k, v} ->
      {interpret(k, opts), interpret(v, opts)}
    end)
  end

  defp interpret_list(ast, opts) do
    Enum.map(ast.children, &interpret(&1, opts))
  end
end
