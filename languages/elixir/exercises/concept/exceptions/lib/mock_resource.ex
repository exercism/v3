defmodule MockResource do
  use Agent

  @spec start() :: {:ok, pid()}
  def start() do
    start_state = %{
      open: MapSet.new(),
      log: [],
      test_resource_error: false
    }

    case Agent.start(fn -> start_state end, name: __MODULE__) do
      {:error, {:already_started, pid}} -> {:ok, pid}
      {:ok, pid} -> {:ok, pid}
    end
  end

  @spec set_test_failure(boolean()) :: :ok
  def set_test_failure(toggle \\ false) when is_boolean(toggle) do
    Agent.update(__MODULE__, fn state ->
      %{state | test_resource_error: toggle}
    end)
  end

  @spec get_log() :: list(String.t())
  def get_log() do
    Agent.get(__MODULE__, fn state ->
      state.log
    end)
  end

  @spec clear_log() :: :ok
  def clear_log() do
    Agent.update(__MODULE__, fn state ->
      %{state | log: []}
    end)
  end

  @spec open(String.t()) :: {:ok, pid()}
  def open(filename \\ "<nil>") when is_binary(filename) do
    resource_pid =
      Agent.get_and_update(__MODULE__, fn
        %{test_resource_error: true} = state ->
          pid = spawn(fn -> nil end)
          {pid, state}

        state ->
          pid = spawn_io_listener()

          {pid,
           %{
             state
             | open: MapSet.put(state.open, pid),
               log: state.log ++ ["[OPEN] #{filename}:#{inspect(pid)}"]
           }}
      end)

    {:ok, resource_pid}
  end

  @spec spawn_io_listener() :: pid()
  def spawn_io_listener() do
    spawn(fn ->
      io_resource_loop()
    end)
  end

  defp io_resource_loop() do
    receive do
      {_io_req, pid, ref, {_request, _, msg}} ->
        Agent.update(__MODULE__, fn state ->
          %{state | log: state.log ++ ["[MSG] #{msg}"]}
        end)

        send(pid, {:io_reply, ref, :ok})
        io_resource_loop()
    end
  end

  @spec get_all_open() :: list(pid())
  def get_all_open() do
    Agent.get(__MODULE__, fn state ->
      MapSet.to_list(state.open)
    end)
  end

  @spec close(pid()) :: :ok
  def close(pid) when is_pid(pid) do
    Agent.update(__MODULE__, fn state ->
      if Process.alive?(pid) do
        Process.exit(pid, :kill)
      end

      %{
        state
        | open: MapSet.delete(state.open, pid),
          log: state.log ++ ["[CLOSE] #{inspect(pid)}"]
      }
    end)
  end

  @spec close_all() :: :ok
  def close_all() do
    get_all_open()
    |> Enum.each(&close/1)
  end
end
