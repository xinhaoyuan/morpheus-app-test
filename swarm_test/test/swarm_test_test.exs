defmodule SwarmTestTest do
  use ExUnit.Case
  doctest SwarmTest

  @tag timeout: 3600_000
  test "Test" do
    # Deadlocks and inconsistent views in test_4
    config = %{
      sched => case System.get_env("SCHED") do
                 nil -> basicpos
                 s -> String.to_atom(s)
               end,
      testcase => case System.get_env("TESTCASE") do
                    nil -> :test_1
                    s -> String.to_atom(s)
                  end,
      repeat => case System.get_env("REPEAT") do
                  nil -> 100
                  s -> String.to_integer(s)
                end
    }
    :io.format(:user, 'Test with config', [config]),
    SwarmTest.test(config)
  end

end
