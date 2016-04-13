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
    new_state = case Map.get(state, channel) do
      nil ->
        Map.put(state, channel, [user])
      users ->
        if Enum.any?(users, &(user == &1)) do
          state
        else
          Map.put(state, channel, [user | users])
        end
    end
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

  defp update_stream(state, channel) do
    users = Map.get(state, channel)
    IO.puts("GOT THE MESSAGE AND WILL START FOLLOWING #{Enum.join(users, ",")} for #{channel}")

    case channel_pid(channel) do
      :undefined ->
        Supervisor.start_child(Twitter.Stream.Supervisor, [{channel, users}])
      pid ->
        Twitter.Stream.Channel.update_stream(pid, users)
    end
  end

  def channel_pid(channel) do
    :gproc.whereis_name(Twitter.Stream.Channel.registered_name(channel))
  end
end

# m = %{"foo" => ["bar"]}
# state = %{"asdf" => ["adampash"]}
# user = "bob"
# Map.get_and_update(
#   state, "asdf",
#   fn users -> {users, List.delete(users, user)} end
# )
# Map.put(m, "foo", List.delete(m["foo"], "bar"))
# update_in(m["foo"], fn arr -> List.delete(arr, "bar") end )
#
# update_in(m["bar"], fn arr -> List.delete(arr, "bar") end )
#
# List.delete(m["foo"], "bar")
# m["foo"]
# update_in(m["foo"], fn arr -> List.delete(arr, "bar") end)
# l = [1,2,3,"foo"]
# List.delete(l, "foo")
