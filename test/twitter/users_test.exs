defmodule Twitter.UsersTest do
  use ExUnit.Case

  test "follows new users" do
    state = %{users: %{}, channels: %{}}
    new_state = Twitter.Users.follow_user("adampash", "123456", "D78UX1", state)
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
    new_state = Twitter.Users.follow_user("adampash", "123456", "D78UX1", state)

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
    new_state = Twitter.Users.unfollow_user(
                  "adampash", "123456", "D78UX1", state
                )

    assert new_state == %{users: %{}, channels: %{}}
  end

  test "clears keys when their value is an empty array" do
    state = %{
      users: %{"123456" => ["D78UX1"], "foo" => []},
      channels: %{"D78UX1" => ["adampash"]}
    }

    new_state = Twitter.Users.clean_empty(state, :users)
    assert new_state == %{
      users: %{"123456" => ["D78UX1"]},
      channels: %{"D78UX1" => ["adampash"]}
    }
  end

  test "genserver api" do
    Twitter.Users.start_link

    Twitter.Users.follow({"adampash", "1234"}, "123456")
    following = Twitter.Users.following("123456")
    assert following == ["adampash"]

    Twitter.Users.follow({"twitter", "12345"}, "123456")
    following = Twitter.Users.following("123456")
    assert "twitter" in following
    assert "adampash" in following
    assert length(following) === 2

    Twitter.Users.unfollow({"twitter", "12345"}, "123456")
    following = Twitter.Users.following("123456")
    assert not "twitter" in following
    assert "adampash" in following
  end
end
