defmodule Twitter.FollowingTest do
  use ExUnit.Case

  test "follows new users" do
    state = %{users: %{}, channels: %{}}
    new_state = Twitter.Following.follow_user("adampash", "123456", "D78UX1", state)
    assert new_state == %{
      users: %{"123456" => ["D78UX1"]},
      channels: %{"D78UX1" => ["adampash"]}
    }
  end

  test "doesn't duplicate users" do
    state = %{
      users: %{"123456" => ["D78UX1"]},
      channels: %{"D78UX1" => ["adampash"]}
    }
    new_state = Twitter.Following.follow_user("adampash", "123456", "D78UX1", state)

    assert new_state == %{
      users: %{"123456" => ["D78UX1"]},
      channels: %{"D78UX1" => ["adampash"]}
    }
  end

  test "unfollows users" do
    state = %{
      users: %{"123456" => ["D78UX1"]},
      channels: %{"D78UX1" => ["adampash"]}
    }
    new_state = Twitter.Following.unfollow_user(
                  "adampash", "123456", "D78UX1", state
                )

    assert new_state == %{users: %{}, channels: %{}}
  end

  test "returns an empty array when not following any users for a room" do
    following = Twitter.Following.users_for_channel("foo", %{channels: %{}, users: %{}})
    assert following == []
  end

  test "returns a username by index number" do
    state = %{
      users: %{"123456" => ["D78UX1"], "2345" => ["D78UX1"]},
      channels: %{"D78UX1" => ["adampash", "bob"]}
    }
    username = Twitter.Following.user_at(1, "D78UX1", state)

    assert username == "bob"
  end
end
