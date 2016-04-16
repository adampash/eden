defmodule Twitter.Users do
  use GenServer

  # External API
  def start_link do
    GenServer.start_link(__MODULE__, %{users: %{}, channels: %{}}, name: __MODULE__)
  end

  def follow({username, user}, channel) do
    GenServer.call(__MODULE__, {:follow, {username, user, channel}})
  end

  def unfollow({username, user}, channel) do
    GenServer.call(__MODULE__, {:unfollow, {username, user, channel}})
  end

  def new_tweet(tweet) do
    GenServer.cast(__MODULE__, {:new_tweet, tweet})
  end

  def following(channel) do
    GenServer.call(__MODULE__, {:following, channel})
  end

  # Internal API

  @doc """
  Takes a user id and a channel, add it to the state as the key for a map.
  The value for the key is a list of channels that are watching
  the user.
  """
  def handle_call({:follow, {username, user, channel}}, _from, state) do
    new_state = follow_user(username, user, channel, state)
    |> stream

    IO.puts "NEW STATE"
    IO.inspect new_state
    {:reply, "Following #{user}", new_state}
  end

  def handle_call({:unfollow, {username, user, channel}}, _from, state) do
    new_state = unfollow_user(username, user, channel, state)
    |> stream

    IO.inspect new_state
    {:reply, "Unfollowed #{user}", new_state}
  end

  def handle_call({:following, channel}, _from, state) do
    usernames = get_in(state, [:channels, channel])

    {:reply, usernames, state}
  end

  def handle_cast({:new_tweet, tweet}, state) do
    user = tweet.user.id_str
    channels = get_in(state, [:users, user])
    Twitter.Message.send(tweet, channels)
    IO.inspect tweet.user.screen_name
    IO.inspect tweet.text
    {:noreply, state}
  end

  def follow_user(username, user, channel, state) do
    state
    |> add_to([:users, user], channel)
    |> add_to([:channels, channel], username)
  end

  defp add_to(state, path, value) do
    state
    |> update_in(path, fn
      nil ->
        [value]
      v ->
        Enum.uniq([value | v])
    end)
  end

  defp remove_from(state, path, value) do
    state
    |> update_in(path, fn
      nil ->
        []
      v ->
        List.delete(v, value)
    end)
  end

  def unfollow_user(username, user, channel, state) do
    state
    |> remove_from([:users, user], channel)
    |> remove_from([:channels, channel], username)
    |> clean_empty(:users)
    |> clean_empty(:channels)
  end

  def clean_empty(state, key) do
    val = Map.get(state, key)
    |> Enum.filter(&_clean_empty/1)
    |> Enum.into(%{})

    Map.put(state, key, val)
  end

  defp _clean_empty({_, []}), do: false
  defp _clean_empty({_, _}), do: true

  defp stream(state) do
    get_in(state, [:users])
    |> Map.keys
    |> stop_stream
    |> start_stream

    state
  end

  defp start_stream(users) do
    stream_pid(0)
    |> _start_stream(users)
  end

  defp _start_stream(:undefined, users) do
    Supervisor.start_child(Twitter.Stream.Supervisor, [{0, users}])
  end

  defp _start_stream(pid, users) do
    Twitter.Stream.update_users(pid, users)
  end

  defp stop_stream(users) do
    stream_pid(0)
    |> kill_stream

    users
  end

  defp kill_stream(:undefined) do
    :ok
  end
  defp kill_stream(pid) do
    Twitter.Stream.stop(pid)
  end

  def stream_pid(id) do
    :gproc.whereis_name(Twitter.Stream.Channel.registered_name(id))
  end
end
