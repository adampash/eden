defmodule Twitter.Follow do
  use GenServer

  # methods to know:
  # user = ExTwitter.user("adampash")
  # id = user.id

  # https://dev.twitter.com/streaming/reference/post/statuses/filter
  # ExTwitter.stream_filter(follow: id)
  #         OR
  # ExTwitter.stream_filter(follow: "adampash")
end
