defmodule Twitter.Stream.Channel do
  use GenServer

  def start_link({channel, _users} = args) do
    GenServer.start_link(__MODULE__, args, name: via_tuple(channel))
  end

  def via_tuple(channel) do
    {:via, :gproc, registered_name(channel)}
  end

  def registered_name(channel) do
    {:n, :l, {channel}}
  end

  def init({channel, users}) do
    Process.flag(:trap_exit, true)
    GenServer.cast(self, {:start_stream, {channel, users}})
    {:ok, {channel, users}}
  end


  def update_stream(pid, users) do
    GenServer.cast(pid, {:start_stream, {users}})
  end

  defp generate_message(pid, tweet, channel) do
    message = %Hedwig.Message{
      robot: pid,
      room: channel,
      type: "message",
      text: "https://twitter.com/#{tweet.user.screen_name}/status/#{tweet.id}"
    }
    {pid, message}
  end

  defp send_tweet({pid, message}) do
    Hedwig.Robot.send(pid, message)
  end


  # Internal API
  def handle_cast({:start_stream, {channel, users}}, {_, _} = state) do
    IO.puts "Starting to follow #{Enum.join(users, ",")}"
    pid = start_stream(channel, users)


    new_state = Tuple.append(state, pid)
    IO.inspect new_state
    {:noreply, new_state}
  end

  def handle_cast({:start_stream, {users}}, {_channel, _users, pid} = state) do
    IO.puts "Kill previous streaming pid"
    # Kill the streaming twitter process; trapped below
    Process.exit(pid, :new_stream)
    # sleep; doing this for the twitter streaming api

    IO.puts "Starting to follow #{Enum.join(users, ",")}"
    # pid = start_stream(channel, users)
    new_state = state
    |> Tuple.delete_at(1)
    |> Tuple.insert_at(1, users)
    # |> Tuple.delete_at(2)
    # |> Tuple.append(pid)
    IO.inspect new_state
    {:noreply, new_state}
  end

  def handle_info({:EXIT, _from, :new_stream}, {channel, users, _} = state) do
    IO.puts "Handling exit for new_stream"
    IO.puts "Starting to follow #{Enum.join(users, ",")}"
    :timer.sleep(100)
    pid = start_stream(channel, users)
    new_state = state
    # |> Tuple.delete_at(1)
    # |> Tuple.insert_at(1, users)
    |> Tuple.delete_at(2)
    |> Tuple.append(pid)
    IO.inspect new_state
    {:noreply, new_state}
  end

  defp start_stream(channel, users) do
    spawn_link(fn ->
      stream = ExTwitter.stream_filter([follow: Enum.join(users, ",")], :infinity)
      # stream = ExTwitter.stream_filter([track: "apple"], :infinity)
      for tweet <- stream do
        IO.puts "Is this tweet by one of our users? #{Enum.join(users, ",")}: #{tweet.user.id}"
        # IO.puts Enum.any?(users, &(&1 == tweet.user.id_str))
        IO.inspect tweet.user
        if Enum.any?(users, &(&1 == tweet.user.id_str)) do
          IO.puts "THIS IS ONE OF OUR USERS"
        # cond do
        #   Enum.any?(users, &(&1 == tweet.user.idStr)) ->
          Hedwig.Registry.whereis(Eden.Robot)
          |> generate_message(tweet, channel)
          |> send_tweet
        end
      end
    end)
  end
end
