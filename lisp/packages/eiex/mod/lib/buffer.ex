defmodule Eiex.Buffer do
  @linebreak ["\n"]

  def enumerate_string(str) do
    Stream.unfold(str, &String.next_grapheme/1)
    |> Stream.chunk_while([:bof],
    fn el, acc ->
      cond do
      el in @linebreak ->
        {:cont, Enum.reverse(acc), [el]}
      true ->
        {:cont, [el | acc]}
      end
    end,
    fn acc ->
      {:cont, Enum.reverse(acc), []}
    end)

  end

  def location(_data, pos) when pos <= 0, do: :bof
  def location(data, pos) do
    endpos = String.length(data)
    lines = enumerate_string(data)
    Enum.reduce_while(lines, {0, 0}, fn el, {row, ctr} ->
      nctr = ctr + length(el)
      cond do
	nctr >= pos ->
          col = pos - ctr
          {:halt, {row, col}}
        nctr == endpos - 1 -> 
          col = pos - ctr
          {:halt, {row, col}}
        true -> {:cont, {row + 1, nctr}}
      end
    end)
  end
  
  def point(data, {row, col}) do
    lines = enumerate_string(data)
    Enum.reduce_while(lines, {0, 0}, fn el, {ctr, crow} ->
      nctr = ctr + length(el)
      cond do
	crow >= row  ->
          {:halt, min(ctr + col, nctr)}
        true -> {:cont, {nctr, row + 1}}
      end
    end)
  end

  def loc_max(string) do
    location(string, String.length(string))
  end
end
