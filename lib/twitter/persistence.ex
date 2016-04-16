defmodule Twitter.Persistence do

  def table(name \\ __MODULE__) do
    :dets.open_file(name, [type: :set])
  end

  def insert(name \\ __MODULE__, key, value) do
    {:ok, name} = :dets.open_file(name, [type: :set])
    :ok = :dets.insert(name, {key, value})
    :dets.close(name)
  end

  def lookup(name \\ __MODULE__, key) do
    {:ok, name} = :dets.open_file(name, [type: :set])
    case :dets.lookup(name, key) do
      [] ->
        :dets.close(name)
        false
      [{^key, value}] ->
        :dets.close(name)
        value
    end
  end

end
