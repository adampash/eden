defmodule Twitter.Stream.Channel do
  use GenServer

  def start_link({channel, _users} = args) do
    # TODO use gproc here to register channel?
    GenServer.start_link(__MODULE__, args, name: via_tuple(channel))
  end

  def via_tuple(channel) do
    {:via, :gproc, registered_name(channel)}
  end

  def registered_name(channel) do
    {:n, :l, {channel}}
  end

  def init({channel, users}) do
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
      text: "https://twitter.com/statuses/#{tweet.id}"
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

    Process.flag(:trap_exit, true)

    new_state = Tuple.append(state, pid)
    IO.inspect new_state
    {:noreply, new_state}
  end

  def handle_cast({:start_stream, {users}}, {channel, _, pid} = state) do
    IO.puts "Kill previous pid"
    Process.exit(pid, :new_stream)
    IO.puts "Starting to follow #{Enum.join(users, ",")}"
    pid = start_stream(channel, users)
    new_state = Tuple.delete_at(state, 2) |> Tuple.append(pid)
    IO.inspect new_state
    {:noreply, new_state}
  end

  def handle_info({:EXIT, _from, :new_stream}, state) do
    IO.puts "Exiting for reason: new_stream"
    {:noreply, state}
  end

  defp start_stream(channel, users) do
    spawn_link(fn ->
      # TODO need to convert usernames to user ids
      stream = ExTwitter.stream_filter([follow: Enum.join(users, ",")], :infinity)
      for tweet <- stream do
        Hedwig.Registry.whereis(Eden.Robot)
        |> generate_message(tweet, channel)
        |> send_tweet
      end
    end)
  end
end
