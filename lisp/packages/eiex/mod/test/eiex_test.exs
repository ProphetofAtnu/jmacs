defmodule EiexTest do
  use ExUnit.Case
  doctest Eiex

  test "greets the world" do
    assert Eiex.hello() == :world
  end
end
