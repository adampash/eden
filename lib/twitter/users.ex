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

  def new_tweet(tweet) do
    GenServer.cast(__MODULE__, {:new_tweet, tweet})
  end

  # Internal API

  @doc """
  Takes a user id and a channel, add it to the state as the key for a map.
  The value for the key is a list of channels that are watching
  the user.
  """
  def handle_call({:follow, {user, channel}}, _from, state) do
    new_state = Map.get(state, user)
    |> follow_user(user, channel, state)
    |> stream

    IO.inspect new_state
    {:reply, "Following #{user}", new_state}
  end

  def handle_call({:unfollow, {user, channel}}, _from, state) do
    new_state = Map.get(state, user)
    |> unfollow_user(user, channel, state)
    |> stream

    IO.inspect new_state
    {:reply, "Unfollowed #{user}", new_state}
  end

  def handle_cast({:new_tweet, tweet}, state) do
    # Hedwig.Registry.whereis(Eden.Robot)
    # |> generate_message(tweet, channel)
    # |> send_tweet
    IO.inspect tweet.user.screen_name
    IO.inspect tweet.text
    {:noreply, state}
  end

  defp follow_user(nil, user, channel, state) do
    Map.put(state, user, [channel])
  end

  defp follow_user(channels, user, channel, state) do
    Map.put(state, user, Enum.uniq([channel | channels]))
  end

  defp unfollow_user(nil, _, _, state) do
    state
  end

  defp unfollow_user(channels, user, channel, state) do
    case List.delete(channels, channel) do
      [] -> Map.delete(state, user)
      channels -> Map.put(state, user, channels)
    end
  end

  defp stream(state) do
    users = Map.keys(state)
    state
    |> stop_stream
    |> start_stream
  end

  defp start_stream(state) do
    users = Map.keys state
    stream_pid(0)
    |> _start_stream(users)
    state
  end

  defp _start_stream(:undefined, users) do
    Supervisor.start_child(Twitter.Stream.Supervisor, [{0, users}])
  end

  defp _start_stream(pid, users) do
    Twitter.Stream.update_users(pid, users)
    # Supervisor.start_child(Twitter.Stream.Supervisor, [{0, users}])
  end

  defp stop_stream(state) do
    stream_pid(0)
    |> kill_stream

    state
  end

  defp kill_stream(:undefined) do
    :ok
  end
  defp kill_stream(pid) do
    Twitter.Stream.stop(pid)
  end

  def stream_pid(id) do
    pid = :gproc.whereis_name(Twitter.Stream.Channel.registered_name(id))
    # {pid, channel, users}
  end
end

  #
  # defp follow_user(nil, user, channel, state) do
  #   Map.put(state, user, [channel])
  # end
  # defp follow_user(channels, user, channel, state) do
  #   Map.put(state, user, Enum.uniq([channel | channels]))
  # end
  #
  # defp update_stream(state) do
  #   users = Map.keys(state)
  #   IO.puts("GOT THE MESSAGE AND WILL START FOLLOWING #{Enum.join(users, ",")} for #{}")
  #
  #   channel_pid(0)
  #   |> start_stream
  # end
  #
  # def channel_pid(id) do
  #   pid = :gproc.whereis_name(Twitter.Stream.Channel.registered_name(id))
  #   {pid, channel, users}
  # end
  #
  # defp start_stream({:undefined, channel, users}) do
  #   # This should eventually support multiple streams with different
  #   # Twitter API keys; 0 here is hard-coding the first of these streams
  #   Supervisor.start_child(Twitter.Stream.Supervisor, [{0, channel, users}])
  # end
  #
  # defp start_stream({pid, channel, users}) do
  #   Supervisor.terminate_child(Twitter.Stream.Supervisor, pid)
  #   # start_stream({:undefined, channel, users})
  #   Twitter.Stream.Channel.update_stream(pid, users)
  # end
# end
