defmodule Twitter.Stream do
  use GenServer

  # External API
  def start_link({id, _users} = args) do
    GenServer.start_link(__MODULE__, args, name: via_tuple(id))
  end

  def via_tuple(id) do
    {:via, :gproc, registered_name(id)}
  end

  def registered_name(id) do
    {:n, :l, {"stream_#{id}"}}
  end

  def init({_, users}) do
    GenServer.cast(self, {:start_stream, users})
    {:ok, {users}}
  end

  def update_users(pid, users) do
    GenServer.call(pid, {:update_users, users})
  end

  def stop(pid) do
    GenServer.call(pid, :stop_stream)
  end

  # Internal API
  def handle_call({:update_users, users}, _from, _state) do
    stream_pid = start_stream(users)
    {:reply, "Starting", {users, stream_pid}}
  end

  def handle_call(:stop_stream, _from, {users, stream_pid}) do
    stop_stream(stream_pid)
    {:reply, "Stopping", {users}}
  end

  def handle_cast({:start_stream, users}, _state) do
    stream_pid = start_stream(users)
    {:noreply, {users, stream_pid}}
  end

  defp start_stream(users) do
    IO.puts "Starting stream for #{Enum.join(users, ",")}..."
    stream = ExTwitter.stream_filter([follow: Enum.join(users, ",")], :infinity)
    spawn_link(fn ->
      for tweet <- stream do
        IO.puts "Is this tweet by one of our users? #{Enum.join(users, ",")}: #{tweet.user.id}"
        # IO.puts Enum.any?(users, &(&1 == tweet.user.id_str))
        IO.inspect tweet.user.screen_name
        if Enum.any?(users, &(&1 == tweet.user.id_str)) do
          IO.puts "THIS IS ONE OF OUR USERS"
          Twitter.Users.new_tweet(tweet)
          # Hedwig.Registry.whereis(Eden.Robot)
          # |> generate_message(tweet, channel)
          # |> send_tweet
        end
      end
    end)

  end

  defp stop_stream(pid) do
    IO.puts "Stopping stream..."
    ExTwitter.stream_control(pid, :stop)
  end
end
