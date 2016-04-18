defmodule Twitter.Persistence do
  use GenServer

  def start_link(db_folder \\ "db") do
    GenServer.start_link(__MODULE__, db_folder, name: __MODULE__)
  end

  def insert(key, data) do
    GenServer.cast(__MODULE__, {:insert, key, data})
  end

  def lookup(key) do
    GenServer.call(__MODULE__, {:lookup, key})
  end

  def init(db_folder) do
    File.mkdir_p(db_folder)
    {:ok, db_folder}
  end

  def handle_cast({:insert, key, data}, db_folder) do
    file_name(db_folder, key)
    |> File.write!(:erlang.term_to_binary(data))
    {:noreply, db_folder}
  end

  def handle_call({:lookup, key}, _, db_folder) do
    data = case File.read(file_name(db_folder, key)) do
      {:ok, contents} -> :erlang.binary_to_term(contents)
      {:error, _reason} -> nil
    end
    {:reply, data, db_folder}
  end

  defp file_name(db_folder, key), do: "#{db_folder}/#{key}"

end
