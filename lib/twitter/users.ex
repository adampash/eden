defmodule Twitter.Users do
  use GenServer
  alias Twitter.Persistence

  # External API
  def start_link do
    initial_state = Persistence.lookup(:followed_users) || Twitter.Following.new
    GenServer.start_link(__MODULE__, initial_state, name: __MODULE__)
  end

  def mass_follow(users, channel) do
    GenServer.call(__MODULE__, {:mass_follow, {users, channel}})
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
  def init(args) do
    {:ok, args}
  end

  @doc """
  Takes a user id and a channel, add it to the state as the key for a map.
  The value for the key is a list of channels that are watching
  the user.
  """
  def handle_call({:follow, {username, user, channel}}, _from, state) do
    new_state = Twitter.Following.follow_user(username, user, channel, state)
    |> stream

    IO.puts "NEW STATE"
    IO.inspect new_state

    Persistence.insert(:followed_users, new_state)
    {:reply, "Following #{user}", new_state}
  end

  def handle_call({:mass_follow, {users, channel}}, _from, state) do
    # users = %{"adam" => {"Adam", "18"}, "bob" => {"Bob", "344"}}
    # Enum.reduce(users, %{}, fn {_, {username, user_id}}, acc -> {username, user_id} end)
    # state = %{channels: %{}, users: %{}}
    # channel = "adf"
    new_state = Enum.reduce(users, state, fn {username, user_id}, acc ->
      Twitter.Following.follow_user(username, user_id, channel, acc)
    end)
    |> stream

    IO.puts "NEW STATE"
    IO.inspect new_state

    Persistence.insert(:followed_users, new_state)
    {:reply, :ok, new_state}
  end

  def handle_call({:unfollow, {username, user, channel}}, _from, state) do
    new_state = Twitter.Following.unfollow_user(username, user, channel, state)
    |> stream

    IO.puts "NEW STATE"
    IO.inspect new_state

    Persistence.insert(:followed_users, new_state)
    {:reply, "Unfollowed #{user}", new_state}
  end

  def handle_call({:following, channel}, _from, state) do
    usernames = Twitter.Following.users_for_channel(channel, state)

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
