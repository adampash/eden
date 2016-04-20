defmodule Twitter.Message do
  def send(tweet, channels) do
    Hedwig.Registry.whereis(Eden.Robot)
    |> generate_messages(tweet, channels)
    |> send_tweets
  end

  defp generate_messages(pid, tweet, channels) do
    Enum.map(channels, fn channel ->
      message = %Hedwig.Message{
        robot: pid,
        room: channel,
        type: "message",
        text: Twitter.Helper.tweet_url(tweet)
      }
      {pid, message}
    end)
  end

  defp send_tweets(messages) do
    Enum.each(messages, &send_tweet/1)
  end

  defp send_tweet({pid, message}) do
    Hedwig.Robot.send(pid, message)
  end
end
