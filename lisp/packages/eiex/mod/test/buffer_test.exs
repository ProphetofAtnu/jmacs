defmodule EiexTest.Buffer do
  use ExUnit.Case
  alias Eiex.Buffer

  @test_buf """
  This is
  a test
  of buffer parsing
  """

  test "translate positions into locations" do
    results =
      for p <- 0..String.length(@test_buf) do
        Buffer.location(@test_buf, p) |> IO.inspect()
      end

    {mrow, mcol} = Enum.reduce(results, {0, 0},
      fn {row, col}, {arow, acol} ->
        {max(row, arow), max(col, acol)}
      end) |> IO.inspect()

    assert mrow == 3
    assert mcol == 17

    assert List.last(results) == {3, 0}
  end

  test "Translates locations into positions" do
    posl = for p <- 0..String.length(@test_buf) do
      Buffer.location(@test_buf, p) |> IO.inspect()
    end

    for {i, l} <- Enum.zip(0..length(posl), posl) do
      assert Buffer.point(@test_buf, l) ==  i
    end

  end
end
