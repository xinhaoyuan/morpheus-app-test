defmodule SwarmTest do

  alias :morpheus_guest, as: G
  alias :morpheus_guest_helper, as: GH

  def black_hole() do
    receive do _ -> black_hole() end
  end

  def test(config) do
    {_ctl, mref} = :morpheus_sandbox.start(__MODULE__, config.testcase, [config],
      [ :monitor,
        {:heartbeat, :once},
        {:fd_opts, [
            :verbose_final,
            {:scheduler,
             {config.sched, [
               ]}}
            ]},
        {:node, :node1@localhost},
        {:clock_limit, config.repeat * 30000 + 10000},
        # :verbose_handle, :verbose_ctl,
      ])
    :success = receive do {:DOWN, ^mref, _, _, reason} -> reason end
  end

  def test_1(config) do
    nodes = [node_1, node_2, node_3] =
      [:node1@localhost, :node2@localhost, :node3@localhost]
    Enum.each(nodes -- [node_1],
      fn node -> GH.bootstrap_remote(node) end)
    GH.bootstrap(node_1)

    Enum.each(nodes,
      fn node ->
        me = self()
        pid = :erlang.spawn(node, fn -> Application.ensure_all_started(:swarm); send(me, self()) end)
        receive do ^pid -> :ok end
      end)

    :timer.sleep(10000)
    :io.format(:user, 'start testing~n', [])

    GH.sync_task(
      [ :repeat, config.repeat,
        fn ->
          # generate pids on each node
          pids = [pid1, pid2, pid3] =
            List.foldr(nodes,
              [],
              fn (node, acc) ->
                pid = :erlang.spawn(node, &black_hole/0)
                ^node = :erlang.node(pid)
                [pid | acc]
              end)

          tab = :ets.new(:test_tab, [:public])

          GH.sync_task(
            [ :par,
              fn ->
                r0 = :rpc.call(node_1, Swarm, :register_name, [:test_proc_1, pid1])
                r1 = :rpc.call(node_1, Swarm, :unregister_name, [:test_proc_1])
                :ets.insert(tab, {:result_1, [r0, r1]})
              end,
              fn ->
                # resetting a node for testing sync and replication
                :ok = :rpc.call(node_2, Application, :stop, [:swarm])
                :ok = :rpc.call(node_2, Application, :start, [:swarm])
              end,
              fn ->
                r0 = :rpc.call(node_3, Swarm, :register_name, [:test_proc_3, pid3])
                r1 = :rpc.call(node_3, Swarm, :unregister_name, [:test_proc_3])
                :ets.insert(tab, {:result_3, [r0, r1]})
              end
            ])

          :timer.sleep(10000)

          {views, _} = :rpc.multicall(nodes, Swarm, :registered, [])
          :io.format(:user, 'result ~p~n', [:ets.tab2list(tab)])
          :io.format(:user, 'views ~p~n', [views])
          # make sure they have the same (and correct) views
          ^views = [[], [], []]

          Enum.each(pids,
            fn pid ->
              mref = Process.monitor(pid)
              Process.exit(pid, :kill)
              receive do
                {:DOWN, ^mref, _, _, _} ->
                  :ok
              end
            end)
          :ets.delete(tab)
        end
      ])

    G.exit_with(:success)
  end

  def test_2(config) do
    nodes = [node_1, node_2, node_3] =
      [:node1@localhost, :node2@localhost, :node3@localhost]
    Enum.each(nodes -- [node_1],
      fn node -> GH.bootstrap_remote(node) end)
    GH.bootstrap(node_1)

    Enum.each(nodes,
      fn node ->
        me = self()
        pid = :erlang.spawn(node, fn -> Application.ensure_all_started(:swarm); send(me, self()) end)
        receive do ^pid -> :ok end
      end)

    :timer.sleep(10000)
    :io.format(:user, 'start testing~n', [])

    GH.sync_task(
      [ :repeat, config.repeat,
        fn ->
          tab = :ets.new(:test_ets, [:public])
          pid = spawn fn ->
            receive do :hello -> :ok end
            :ets.insert(tab, {:message_delivered, true})
            true = self() in Swarm.members(:test_group)
            Swarm.leave(:test_group, self())
            :timer.sleep(5000)
            [] = Swarm.members(:test_group)
          end

          Swarm.register_name(:test_proc, pid)

          GH.sync_task(
            [ :par,
              fn ->
                # resetting a node for testing sync and replication
                :ok = :rpc.call(node_1, Swarm, :join, [:test_group, pid])
              end,
              fn ->
                # resetting a node for testing sync and replication
                :ok = :rpc.call(node_2, Application, :stop, [:swarm])
                :ok = :rpc.call(node_2, Application, :start, [:swarm])
              end,
              fn ->
                :ok = :rpc.call(node_3, Swarm, :publish, [:test_group, :hello])
              end
            ])

          :timer.sleep(10000)

          case :ets.lookup(tab, :message_delivered) do
            [] ->
              {[x, x, x], []} = :rpc.multicall(nodes, Swarm, :members, [:test_group])
            _ ->
              {[[], [], []], []} = :rpc.multicall(nodes, Swarm, :members, [:test_group])
          end

          mref = Process.monitor(pid)
          Process.exit(pid, :kill)
          receive do
            {:DOWN, ^mref, _, _, _} ->
              :ok
          end
          :ets.delete(tab)
        end
      ])

    G.exit_with(:success)
  end

  def test_3(config) do
    nodes = [node_1, node_2, node_3] =
      [:node1@localhost, :node2@localhost, :node3@localhost]
    Enum.each(nodes -- [node_1],
      fn node -> GH.bootstrap_remote(node) end)
    GH.bootstrap(node_1)

    Enum.each(nodes,
      fn node ->
        me = self()
        pid = :erlang.spawn(node, fn -> Application.ensure_all_started(:swarm); send(me, self()) end)
        receive do ^pid -> :ok end
      end)

    :timer.sleep(10000)
    :io.format(:user, 'start testing~n', [])

    GH.sync_task(
      [ :repeat, config.repeat,
        fn ->
          tab = :ets.new(:test_ets, [:public])
          pid1 = spawn &black_hole/0
          pid2 = spawn &black_hole/0

          :yes = Swarm.register_name(:test_proc, pid1)
          :yes = Swarm.register_name(:test_proc_2, pid2)
          Application.stop(:swarm)
          Process.exit(pid1, :kill)
          Application.start(:swarm)
          :timer.sleep(10000)

          {[[test_proc_2: ^pid2], [test_proc_2: ^pid2], [test_proc_2: ^pid2]], []} = :rpc.multicall(nodes, Swarm, :registered, [])


          Enum.each([pid1, pid2],
            fn pid ->
              mref = Process.monitor(pid)
              Process.exit(pid, :kill)
              receive do
                {:DOWN, ^mref, _, _, _} ->
                  :ok
              end
            end)
          :ets.delete(tab)
        end
      ])

    G.exit_with(:success)
  end

  def test_4(config) do
    nodes = [node_1, node_2, node_3] =
      [:node1@localhost, :node2@localhost, :node3@localhost]
    Enum.each(nodes -- [node_1],
      fn node -> GH.bootstrap_remote(node) end)
    GH.bootstrap(node_1)

    Enum.each(nodes,
      fn node ->
        me = self()
        pid = :erlang.spawn(node, fn -> Application.ensure_all_started(:swarm); send(me, self()) end)
        receive do ^pid -> :ok end
      end)

    :timer.sleep(10000)
    :io.format(:user, 'start testing~n', [])

    GH.sync_task(
      [ :repeat, config.repeat,
        fn ->
          # generate pids on each node
          pids = [pid1, pid2, pid3] =
            List.foldr(nodes,
              [],
              fn (node, acc) ->
                pid = :erlang.spawn(node, &black_hole/0)
                ^node = :erlang.node(pid)
                [pid | acc]
              end)

          tab = :ets.new(:test_tab, [:public])

          GH.sync_task(
            [ :par,
              fn ->
                r0 = :rpc.call(node_1, Swarm, :register_name, [:test_proc, pid1])
                :ets.insert(tab, {:result_1, [r0]})
              end,
              fn ->
                r0 = :rpc.call(node_3, Swarm, :register_name, [:test_proc, pid3])
                :ok = :rpc.call(node_3, Application, :stop, [:swarm])
                :ok = :rpc.call(node_3, Application, :start, [:swarm])
                :ets.insert(tab, {:result_3, [r0]})
              end
            ])

          :timer.sleep(10000)

          {views, _} = :rpc.multicall(nodes, Swarm, :registered, [])
          :io.format(:user, 'pids ~p~n', [pids])
          :io.format(:user, 'result ~p~n', [:ets.tab2list(tab)])
          :io.format(:user, 'views ~p~n', [views])
          # make sure they have the same views
          [x, x, x] = views
          case {:ets.lookup(tab, :result_1), :ets.lookup(tab, :result_3)} do
            {[{_, [:yes]}],[{_, [:yes]}]} ->
              :ok
            {[{_, [:yes]}],[{_, [:no]}]} ->
              ^x = [{:test_proc, pid1}]
            {[{_, [:no]}],[{_, [:yes]}]} ->
              ^x = [{:test_proc, pid3}]
            _Other ->
              :erlang.error(:unexpected, _Other)
          end

          Enum.each(pids,
            fn pid ->
              mref = Process.monitor(pid)
              Process.exit(pid, :kill)
              receive do
                {:DOWN, ^mref, _, _, _} ->
                  :ok
              end
            end)
          :ets.delete(tab)
        end
      ])

    G.exit_with(:success)
  end


end
