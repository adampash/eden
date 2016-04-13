defmodule Twitter.Stream.Channel do
  use GenServer

  def start_link(args) do
    # TODO use gproc here to register channel?
    GenServer.start_link(__MODULE__, args)
  end

  def init(_opts) do
    spawn_link(fn ->
      # ExTwitter.stream_filter(follow: "adampash")
      stream = ExTwitter.stream_filter(track: "apple")
      for tweet <- stream do
        Hedwig.Registry.whereis(Eden.Robot)
        |> generate_message(tweet)
        |> send_tweet
      end
    end)

    {:ok, []}
  end

  defp generate_message(pid, tweet) do
    message = %Hedwig.Message{
      robot: pid,
      room: "D0ZP38MEU",
      type: "message",
      text: "https://twitter.com/statuses/#{tweet.id}"
    }
    {pid, message}
  end

  defp send_tweet({pid, message}) do
    Hedwig.Robot.send(pid, message)
  end

end
