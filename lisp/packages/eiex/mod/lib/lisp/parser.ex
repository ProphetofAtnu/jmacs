defmodule Eiex.Lisp.Parser do
  @delimiters ["(", "{", "[", "]", "}", ")"]
  @opens ["(", "{", "["]
  @closes [")", "}", "]"]
  @closes_to_opens Enum.zip(@closes, @opens) |> Enum.into(%{})

  alias Eiex.Lisp.AST, as: AST

  defmodule State do
    defstruct level: 0, children: [], parent: nil, marker: nil
  end

  defp handle_token(elem, {:escape, acc}) do
    case acc do
      {x, acc} -> {[], {x, [elem | acc]}}
      acc -> {[], [elem | acc]}
    end
  end

  defp handle_token(elem, {:string, acc}) do
    case elem do
      "\\" -> {[], {:escape, {:string, acc}}}
      "\"" -> {[Enum.reverse(["\"" | acc]) |> IO.iodata_to_binary()], []}
      c -> {[], {:string, [c | acc]}}
    end
  end

  defp handle_token(elem, acc) do
    cond do
      elem == "\\" ->
        {[], {:escape, acc}}

      elem == "\"" ->
        {[], {:string, [elem | acc]}}

      :binary.first(elem)
      |> :unicode_util.is_whitespace() ->
        case acc do
          [] -> {[], []}
          _ -> {[Enum.reverse(acc) |> IO.iodata_to_binary()], []}
        end

      elem in @delimiters ->
        case acc do
          [] -> {[elem], []}
          _ -> {[Enum.reverse(acc) |> IO.iodata_to_binary(), elem], []}
        end

      true ->
        {[], [elem | acc]}
    end
  end

  def tokenize_string(data) do
    {pre, post} =
      Stream.unfold(data, &String.next_grapheme/1)
      |> Enum.flat_map_reduce(
        [],
        &handle_token/2
      )

    if length(post) > 0 do
      pre ++ [IO.iodata_to_binary(Enum.reverse(post))]
    else
      pre
    end
  end

  def get_open(delim) do
    Map.fetch!(@closes_to_opens, delim)
  end

  defp clean_state(state) do
    Map.merge(
      %AST{},
      Map.delete(state, :__struct__)
      |> Map.delete(:parent)
      |> Map.update(:children, [], &Enum.reverse/1)
    )
  end

  def parse_token(token, state \\ %State{}) do
    %State{
      level: level,
      marker: marker,
      parent: parent
    } = state

    case token do
      n when n in @opens ->
        {:ok, %State{parent: state, level: level + 1, marker: n}}

      n when n in @closes ->
        if get_open(n) == marker do
          {:ok,
           %State{
             parent
             | children: [
                 clean_state(state)
                 | parent.children
               ]
           }}
        else
          {:error, {:unmatched, marker, level}}
        end

      n ->
        {:ok, %State{state | children: [n | state.children]}}
    end
  end

  def parse_tokens(tokens, acc \\ %State{})

  def parse_tokens([el | rest], acc) do
    with {:ok, state} <- parse_token(el, acc) do
      parse_tokens(rest, state)
    else
      e -> e
    end
  end

  def parse_tokens([], acc) do
    if acc.level != 0 do
      {:error, {:unmatched, acc.marker, acc.level}}
    else
      {:ok, clean_state(acc)}
    end
  end

  def parse(data) do
    tokenize_string(data) |> parse_tokens()
  end
end
