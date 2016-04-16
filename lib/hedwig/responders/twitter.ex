defmodule Hedwig.Responders.Twitter do

  use Hedwig.Responder

  @usage """
  eden follow ev - follows a user and puts tweets in the current channel (or DMs them to you if you're not in a channel)
  """
  respond ~r/follow\s+(\w+)/i, msg do
    username = msg.matches[1]
    resp = case ExTwitter.user_search(username, count: 1, include_entities: false) do
      [] ->
        "Hmm... couldn't find a twitter user called #{username} :thinking_face:"
      [user] ->
        Twitter.Users.follow({user.screen_name, user.id_str}, msg.room)
        link = "https://twitter.com/#{user.screen_name}/status/#{user.status.id}"
        """
          You got it. I'll start following `#{username}` and posting new tweets to this channel.
          Here's the latest from `#{username}`:
          #{link}
        """
    end
    reply msg, resp
  end

  @usage """
  eden unfollow ev - unfollows a user from the current channel (or your DMs)
  """
  respond ~r/unfollow\s+(\w+)/i, msg do
    username = msg.matches[1]
    resp = case ExTwitter.user_search(username, count: 1, include_entities: false) do
      [] ->
        "Hmm... I couldn't find a twitter user called #{username} :thinking_face:"
      [user] ->
        Twitter.Users.unfollow({user.screen_name, user.id_str}, msg.room)
        "you got it. I'll stop following #{msg.matches[1]} for this channel"
    end
    reply msg, resp
  end

  @usage """
  eden following - lists the twitter users followed by the current channel
  """
  respond ~r/.*following/, msg do
    users = Twitter.Users.following(msg.room)
    |> Stream.with_index(1)
    |> Enum.map(fn {k, v} -> "\n#{v}. #{k}" end)
    reply msg, "here are the twitter users I'm following for this channel:\n#{users}"
  end
end
