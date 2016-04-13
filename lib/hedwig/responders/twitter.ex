defmodule Hedwig.Responders.Twitter do

  use Hedwig.Responder

  @usage """
  eden follow ev - follows a user and puts tweets in the current channel (or DMs them to you if you're not in a channel)
  """
  respond ~r/follow\s+(\w+)/i, msg do
    Twitter.Users.follow(msg.matches[1], msg.room)
    reply msg,
    """
      you got it. I'll start following #{msg.matches[1]} and posting new tweets to this channel.
      here's the latest from #{msg.matches[1]}:
    """
  end

  @usage """
  eden unfollow @ev - unfollows a user from the current channel (or your DMs)
  """
  respond ~r/unfollow\s+(\w+)/i, msg do
    reply msg, "you got it. I'll stop following #{msg.matches[1]} for this channel"
  end

  @usage """
  eden list twitter - lists the twitter users being followed in the current channel
  """
  respond ~r/list/i, msg do
    reply msg, "here are the twitter users being followed by this channel:"
  end
end
