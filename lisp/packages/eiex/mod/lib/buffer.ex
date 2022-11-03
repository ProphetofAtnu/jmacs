defmodule Eiex.Buffer do
  @linebreak ["\n"]

  def enumerate_string(str) do
    Stream.unfold(str, &String.next_grapheme/1)
  end

  def location(data, pos) do
    section =
    if pos < 0 do
      enumerate_string(data)
    else 
      enumerate_string(data)
      |> Enum.take(pos)
    end

    end_buf =
      Enum.reverse(section)
      |> Enum.take_while(&(&1 != @linebreak))

    {Enum.count(section, &(&1 == @linebreak)), Enum.count(end_buf)}
  end

  def point(data, {row, col}) do
    case enumerate_string(data)
    |> Enum.reduce_while({0, 0, 0}, fn c, {rows, cols, ctr} ->
      if {rows, cols} == {row, col} || rows > row do
        {:halt, ctr}
      else
        case c do
          "\n" -> {:cont, {rows + 1, 0, ctr + 1}}
          _ -> {:cont, {rows, cols + 1, ctr + 1}}
        end
      end
    end) do
      {_, _, acc} -> acc
      acc -> acc
    end
  end

  def location_max(data), do: location(data, -1)
  def point_max(data), do: String.length(data)

  def suggest(data, pos \\ -1)

  def suggest(data, {row, col}) do
    ElixirSense.suggestions(data, row, col + 1)
  end

  def suggest(data, pos) when is_integer(pos) do
    suggest(data, location(data, pos))
  end

end
