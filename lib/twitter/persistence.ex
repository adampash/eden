defmodule Twitter.Persistence do
  use GenServer

  def start_link(db_folder \\ "db") do
    # :timer.send_interval(__MODULE__, :archive, 1000)
    GenServer.start_link(__MODULE__, db_folder, name: __MODULE__)
  end

  def insert(key, data) do
    GenServer.cast(__MODULE__, {:insert, key, data})
  end

  def lookup(key) do
    GenServer.call(__MODULE__, {:lookup, key})
  end

  def archive do
    GenServer.call(__MODULE__, :archive)
  end

  def init(db_folder) do
    File.mkdir_p(db_folder)
    one_day = 86_400_000 # milliseconds
    :timer.send_interval(one_day, __MODULE__, :archive)
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

  def handle_info(:archive, db_folder) do
    IO.puts "Running daily archive"

    File.mkdir_p("#{db_folder}/archive")
    {:ok, files} = File.ls(db_folder)

    files
    |> Enum.filter(&(File.regular?("#{db_folder}/#{&1}")))
    |> archive_files(db_folder)

    {:noreply, db_folder}
  end

  defp archive_files(files, db_folder) do
    for file <- files, do: archive(file, db_folder)
  end

  defp archive(file, db_folder) do
    {{year, month, day}, _} = :calendar.universal_time
    File.copy(file_name(db_folder, file), "#{db_folder}/archive/#{year}_#{month}_#{day}_#{file}")
  end

  defp file_name(db_folder, key), do: "#{db_folder}/#{key}"

end
