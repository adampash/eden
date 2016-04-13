defmodule Twitter.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      # worker(Twitter.Follow, []),
      supervisor(Twitter.Stream.Supervisor, [])
    ]

    supervise(children, strategy: :one_for_one, name: __MODULE__)
  end
end
