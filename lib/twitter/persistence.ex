defmodule Twitter.Persistence do

  def table(name \\ __MODULE__) do
    open_table(name)
  end

  def insert(name \\ __MODULE__, key, value) do
    {:ok, name} = open_table(name)
    :ok = :dets.insert(name, {key, value})
    close_table(name)
  end

  def lookup(name \\ __MODULE__, key) do
    {:ok, name} = open_table(name)
    case :dets.lookup(name, key) do
      [] ->
        close_table(name)
        false
      [{^key, value}] ->
        close_table(name)
        value
    end
  end

  defp open_table(name) do
    :dets.open_file(db_name(name), [type: :set])
  end

  defp close_table(name) do
    :dets.close(db_name(name))
  end

  defp db_name(name) do
    "db/#{name}"
  end
end
