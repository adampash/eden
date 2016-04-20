defmodule Twitter.PersistenceTest do
  use ExUnit.Case

  setup_all do
    on_exit fn ->
      File.rm("db/test_key")
    end
  end

  test "inserts and retrieves values" do
    Twitter.Persistence.start_link
    inserted = %{foo: "bar"}
    Twitter.Persistence.insert(:test_key, inserted)

    retrieved = Twitter.Persistence.lookup(:test_key)
    assert retrieved == inserted
  end

  test "returns false if no value stored" do
    Twitter.Persistence.start_link
    retrieved = Twitter.Persistence.lookup(:not_stored)
    assert is_nil(retrieved)
  end
end
