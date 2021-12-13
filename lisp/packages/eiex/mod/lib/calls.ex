defmodule Eiex.Calls do

  def call(:test, state) do
    Eiex.IO.send(:ok)
    state
  end

  def call({:echo, term}, state) do
    Eiex.IO.send(term)
    state
  end

  def call({:invoke, module, function, args}, state) do
    Eiex.IO.send(apply(module, function, args))
    state
  end

  def call({:invoke, function, args}, state) do
    apply(function, args)
    state
  end

  def call(_, state) do
    state
  end

end
