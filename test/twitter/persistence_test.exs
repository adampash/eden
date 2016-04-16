defmodule Twitter.PersistenceTest do
  use ExUnit.Case

  test "can set up a database" do
    {:ok, table} = Twitter.Persistence.table(:test_table)
    assert table == :test_table
  end

  test "inserts and retrieves values" do
    inserted = %{foo: "bar"}
    Twitter.Persistence.insert(:test_table, :test_key, inserted)

    retrieved = Twitter.Persistence.lookup(:test_table, :test_key)
    assert retrieved == inserted
  end

  test "returns false if no value stored" do
    retrieved = Twitter.Persistence.lookup(:test_table, :not_stored)
    assert not retrieved
  end
end
