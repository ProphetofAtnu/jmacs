defmodule Eiex.Lisp do
  alias Eiex.Lisp.{AST, Parser}

  def translate(data, opts \\ []) do 
    with {:ok, ast} <- Parser.parse(data) do
    {:ok, AST.interpret(ast, opts)}
    else
      e -> e
    end
  end


end
