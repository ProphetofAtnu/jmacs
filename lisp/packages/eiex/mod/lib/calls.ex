defmodule Eiex.Calls do
  use Task

  alias Eiex.Server

  def start_link(arg) do
    Task.start_link(__MODULE__, :call, [arg])
  end

  def call(:test) do
    Server.schedule_send(:ok)
  end

  def call({:echo, term}) do
    Server.schedule_send(term)
  end

  # def call({:invoke, module, function, args}, state) do
  #   Eiex.IO.send(apply(module, function, args))
  #   state
  # end

  # def call({:invoke, function, args}, state) do
  #   apply(function, args)
  #   state
  # end

  def call({:complete, arg}) do
    Eiex.Complete.complete(arg)
    |> Server.schedule_send()
  end

  def call(_) do
    Server.schedule_send(:noop)
  end

end
