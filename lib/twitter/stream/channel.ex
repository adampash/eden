defmodule Twitter.Stream.Channel do
  use GenServer

  def start_link({id, _channel, _users} = args) do
    GenServer.start_link(__MODULE__, args, name: via_tuple(id))
  end

  def via_tuple(id) do
    {:via, :gproc, registered_name(id)}
  end

  def registered_name(id) do
    {:n, :l, {"stream_#{id}"}}
  end

  def init({_, channel, users}) do
    GenServer.cast(self, {:start_stream, {channel, users}})
    {:ok, {channel, users}}
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
