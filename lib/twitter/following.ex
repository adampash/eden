defmodule Twitter.Following do

  def new do
    %{users: %{}, channels: %{}}
  end

  def follow_user(username, user, channel, state) do
    state
    |> add_to([:users, user], channel)
    |> add_to([:channels, channel], username)
  end

  def users_for_channel(channel, state) do
    get_in(state, [:channels, channel]) || []
  end

  def unfollow_user(username, user, channel, state) do
    state
    |> remove_from([:users, user], channel)
    |> remove_from([:channels, channel], username)
    |> clean_empty(:users)
    |> clean_empty(:channels)
  end

  defp clean_empty(state, key) do
    val = Map.get(state, key)
    |> Enum.filter(&_clean_empty/1)
    |> Enum.into(%{})

    Map.put(state, key, val)
  end

  defp add_to(state, path, value) do
    state
    |> update_in(path, fn
      nil ->
        [value]
      v ->
        Enum.uniq([value | v])
    end)
  end

  defp remove_from(state, path, value) do
    state
    |> update_in(path, fn
      nil ->
        []
      v ->
        List.delete(v, value)
    end)
  end

  defp _clean_empty({_, []}), do: false
  defp _clean_empty({_, _}), do: true
end
