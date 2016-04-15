defmodule Twitter.Users do
  use GenServer

  # External API

  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def follow(user, channel) do
    GenServer.call(__MODULE__, {:follow, {user, channel}})
  end

  def unfollow(user, channel) do
    GenServer.call(__MODULE__, {:unfollow, {user, channel}})
  end

  # Internal API

  def handle_call({:follow, {user, channel}}, _from, state) do
    new_state = Map.get(state, channel)
    |> follow_user(user, channel, state)
    update_stream(new_state, channel)
    IO.inspect new_state
    {:reply, "Following #{user}", new_state}
  end

  def handle_call({:unfollow, {user, channel}}, _from, state) do
    new_state = case Map.get(state, channel) do
      nil ->
        state
      users ->
        Map.put(state, channel, List.delete(users, user))
    end
    update_stream(new_state, channel)
    IO.inspect new_state
    {:reply, "Unfollowed #{user}", new_state}
  end

  defp follow_user(nil, user, channel, state) do
    Map.put(state, channel, [user])
  end
  defp follow_user(users, user, channel, state) do
    Map.put(state, channel, Enum.uniq([user | users]))
  end

  defp update_stream(state, channel) do
    users = Map.get(state, channel)
    IO.puts("GOT THE MESSAGE AND WILL START FOLLOWING #{Enum.join(users, ",")} for #{channel}")

    channel_pid({channel, users})
    |> stream
  end

  def channel_pid({channel, users}) do
    pid = :gproc.whereis_name(Twitter.Stream.Channel.registered_name(channel))
    {pid, channel, users}
  end

  defp stream({:undefined, channel, users}) do
    Supervisor.start_child(Twitter.Stream.Supervisor, [{channel, users}])
  end

  defp stream({pid, channel, users}) do
    Supervisor.terminate_child(Twitter.Stream.Supervisor, pid)
    stream({:undefined, channel, users})
    # Twitter.Stream.Channel.update_stream(pid, users)
  end
end
