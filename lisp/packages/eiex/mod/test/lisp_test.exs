defmodule EiexTest.Lisp do
  use ExUnit.Case
  import Eiex.Lisp.Parser

  alias Eiex.Lisp
  test "Parser tokenizes strings" do
    str = "(this is {a test} [case] \"with \\\"a\\\" string\")"

    result = [
      "(",
      "this",
      "is",
      "{",
      "a",
      "test",
      "}",
      "[",
      "case",
      "]",
      "\"with \"a\" string\"",
      ")"
    ]

    # IO.inspect(parse(str))
    assert(tokenize_string(str) == result)
  end

  test "Parser creates AST trees" do
    str = "(this is {a test} [case])"

    {:ok, _result} = parse(str)
  end


  test "translate singleton" do
    str = "(this ((is . a)) (:a \"test\") [case])"
    parse(str) |> IO.inspect()

  end
  
  test "translate arbitrary lisp" do
    str = "(this ((is . a)) (:a \"test\") [case])"

    {:ok, [atm, mp, kwl, tup]} = Lisp.translate(str) |> IO.inspect()

    assert is_atom(atm)
    assert is_map(mp)
    assert is_list(kwl) && Keyword.fetch!(kwl, :a)
    assert is_tuple(tup)

  end

end
