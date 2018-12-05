defmodule SwarmTestSimple do

  defp spawn_node(name) do
    {:ok, node} = :slave.start('localhost', name)
    :rpc.call(node, :code, :add_paths, [:code.get_path()])
    for {app_name, _, _} <- Application.loaded_applications() do
      for {key, val} <- Application.get_all_env(app_name) do
        :rpc.call(node, Application, :put_env, [app_name, key, val])
      end
    end
    :rpc.call(node, Application, :ensure_all_started, [:mix])
    :rpc.call(node, Mix, :env, [Mix.env()])
    for {app_name, _, _} <- Application.started_applications() do
      :rpc.call(node, Application, :ensure_all_started, [app_name])
    end
    node
  end

  defp stop_node(node) do
    :slave.stop(node)
  end

  def black_hole() do
    receive do _ -> black_hole() end
  end

  def test() do
    {:ok, _} = Node.start(:master@localhost, :shortnames)

    node1 = spawn_node(:node1)
    node2 = spawn_node(:node2)
    node3 = spawn_node(:node3)

    nodes = [node1, node2, node3]

    # Ignore this node (master) in swarm
    Enum.each(nodes, fn node ->
      :ok = :rpc.call(node, Application, :put_env, [:swarm, :node_blacklist, [~r/^master@.*$/]])
    end)

    :ok = :rpc.call(node1, Application, :put_env, [:swarm, :sync_nodes_timeout, 5000])
    :ok = :rpc.call(node2, Application, :put_env, [:swarm, :sync_nodes_timeout, 5000])
    :ok = :rpc.call(node3, Application, :put_env, [:swarm, :sync_nodes_timeout, 5000])

    Enum.each(nodes, fn node ->
      {:ok, _} = :rpc.call(node, Application, :ensure_all_started, [:swarm])
    end)

    {:ok, dummy_pid} = :rpc.call(node1, MyApp.Worker, :start_link, [])

    {pid1, mref1} = spawn_monitor fn ->
      r0 = :rpc.call(node1, Swarm, :register_name, [:test_proc, dummy_pid])
      :io.format(:user, "register_name -> ~p~n", [r0])
    end

    # {pid2, mref2} = spawn_monitor fn ->
    #   :ok = :rpc.call(node2, Application, :stop, [:swarm])
    #   :ok = :rpc.call(node2, Application, :start, [:swarm])
    # end

    :io.format("wait for jobs to finish~n", [])

    receive do
      {:DOWN, ^mref1, _, _, _} -> :ok
    end

    # receive do
    #   {:DOWN, ^mref2, _, _, _} -> :ok
    # end

    :io.format("all done~n", [])

    stop_node(node1)
    stop_node(node2)
    stop_node(node3)
  end

end

defmodule MyApp.Worker do
  def start_link(), do: GenServer.start_link(__MODULE__, [])

  def init(_name) do
    # IO.inspect "starting #{inspect self()} on #{Node.self}"
    {:ok, {:rand.uniform(5_000), 0}, 0}
  end

  def handle_call({:swarm, :begin_handoff}, _from, {delay, count}) do
    {:reply, {:resume, {delay, count}}, {delay, count}}
  end

  def handle_call(:ping, _from, state) do
    {:reply, {:pong, self()}, state}
  end

  def handle_cast({:swarm, :end_handoff, {delay, count}}, {_, _}) do
    {:noreply, {delay, count}}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info(:timeout, {delay, count}) do
    Process.send_after(self(), :timeout, delay)
    {:noreply, {delay, count + 1}}
  end

  # this message is sent when this process should die
  # because it's being moved, use this as an opportunity
  # to clean up
  def handle_info({:swarm, :die}, state) do
    {:stop, :shutdown, state}
  end

  def handle_info(_, state), do: {:noreply, state}

  def terminate(_reason, _state) do
    # IO.inspect "stopping #{inspect self()} on #{Node.self}"
    :ok
  end
end
