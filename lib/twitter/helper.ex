defmodule Twitter.Helper do
  def tweet_url(tweet) do
    tweet_url(tweet, tweet.user.screen_name)
  end

  def tweet_url(tweet, username) do
    "https://twitter.com/#{username}/status/#{tweet.id}"
  end
end
