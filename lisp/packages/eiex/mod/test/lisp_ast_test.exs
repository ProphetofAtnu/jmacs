defmodule EiexTest.Lisp.AST do
  use ExUnit.Case
  import Eiex.Lisp.Parser
  alias Eiex.Lisp.AST, as: AST

  test "AST node utils works" do
    str = "(this is {a (blah) test} [case])"
    {:ok, result} = parse(str)

    assert AST.is_root?(result)

    max_level =
      AST.walk(result, 0, fn x, acc ->
        if AST.is_node?(x) && Map.get(x, :level, 0) > acc do
          Map.get(x, :level)
        else
          acc
        end
      end)

    assert(max_level == 3)

    item_count =
      AST.walk(result, 0, fn _x, acc ->
        acc + 1
      end)

    assert(item_count == 11)

    node_count =
      AST.walk(result, 0, fn x, acc ->
        if AST.is_node?(x) do
          acc + 1
        else
          acc
        end
      end)

    assert(node_count == 5)
  end

  test "AST primitives" do
    assert AST.interpret("\"test\"") == "test"
    assert AST.interpret("1.11") == 1.11
    assert AST.interpret("123") == 123
    assert AST.interpret("atom") == :atom
  end
  
  test "AST Data structures" do
    {:ok, result} = parse("((this . \"is\") (a . \"test\"))")
    ast = result |> Map.fetch!(:children) |> hd

    IO.inspect(ast)

    assert is_map(AST.interpret(ast) |> IO.inspect())
  end

  test "AST plist" do
    {:ok, plist} = parse("(:this is :a test)")

    plist_res = AST.interpret(plist) |> IO.inspect()
    assert plist_res == [this: :is, a: :test]

    list_res = AST.interpret(plist, [:no_plists]) |> IO.inspect()
    assert is_list(list_res) && list_res == [:this, :is, :a, :test]
  end

  test "AST vector" do
    {:ok, vec} = parse("[this is a test]")
    IO.inspect(vec)

    assert AST.interpret(vec) |> IO.inspect() == {:this, :is, :a, :test}
  end

  test "AST Pairs" do
    {:ok, pair} = parse("(pair . (test thing))")
    IO.inspect(pair)

    AST.interpret(pair) |> IO.inspect()
  end
end
