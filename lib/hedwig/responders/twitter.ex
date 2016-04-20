defmodule Hedwig.Responders.Twitter do

  use Hedwig.Responder

  @usage """
  eden follow ev - follows a user and puts tweets in the current channel \
  (or DMs them to you if you're not in a channel)
  """
  respond ~r/follow\s+(.+)$/i, msg do
    usernames = String.split(msg.matches[1], ~r/\s?,/)
    |> Enum.map(&String.strip/1)

    users = usernames
    |> Enum.join(",")
    |> ExTwitter.user_lookup

    usermap = usernames
    |> Enum.reduce(%{}, fn username, acc ->
      val = Enum.find(users, fn u ->
        String.downcase(username) == String.downcase(u.screen_name)
      end)
      case val do
        nil -> Map.put(acc, username, nil)
        user -> Map.put(acc, username, {user.screen_name, user.id_str})
      end
    end)
    |> follow_users(msg.room)

    found_users = usermap
    |> Enum.filter(fn {_, v} -> is_tuple(v) end)
    |> Enum.into(%{})
    |> Map.keys

    not_found = usermap
    |> Enum.filter(fn {_, v} -> is_nil(v) end)
    |> Enum.into(%{})
    |> Map.keys

    resp = found_users_message(found_users)
    |> not_found_users_message(not_found)

    reply msg, resp
  end

  @usage """
  eden unfollow ev - unfollows a user from the current channel (or your DMs)
  """
  respond ~r/unfollow\s+(\w+)/i, msg do
    username = cond do
      msg.matches[1] =~ ~r/^\d+$/ -> # it's a digit
        index = String.to_integer(msg.matches[1]) - 1
        Twitter.Users.get_user_at(index, msg.room)
      true ->
        msg.matches[1]
    end
    user = ExTwitter.user(username)
    Twitter.Users.unfollow({user.screen_name, user.id_str}, msg.room)
    resp = "you got it. I'll stop following *#{user.screen_name}* for this channel"

    reply msg, resp
  end

  @usage """
  eden following - lists the twitter users followed by the current channel
  """
  respond ~r/.*following/, msg do
    users = Twitter.Users.following(msg.room)
    |> Stream.with_index(1)
    |> Enum.map(fn {k, v} -> "\n#{v}. #{k}" end)

    resp = case users do
      [] -> """
            It doesn't look like you're following any twitter users in this \
            channel. To follow someone, type, e.g., `eden follow potus`
            """
      users -> "here are the twitter users I'm following for this channel:\n#{users}"
    end
    reply msg, resp
  end

  defp follow_users(usermap, room) do
    usermap
    |> Map.values
    |> Enum.filter(&is_tuple/1)
    |> Twitter.Users.mass_follow(room)

    usermap
  end

  defp found_users_message([]), do: ""
  defp found_users_message([username]) do
    tweet = ExTwitter.user(username).status
    """
    You got it. I'll start following *#{username}* and posting \
    new tweets to this channel.

    Here's the latest from #{username}:

    #{Twitter.Helper.tweet_url(tweet, username)}
    """
  end
  defp found_users_message(users) do
    """
    You got it. I'll start following *#{Enum.join(users, ", ")}* and posting \
    new tweets to this channel.
    """
  end

  defp not_found_users_message(msg, []), do: msg
  defp not_found_users_message(msg, users) do
    msg <> """

    Unfortunately I couldn't find any users matching *#{Enum.join(users, ", ")}*
    """
  end
end
