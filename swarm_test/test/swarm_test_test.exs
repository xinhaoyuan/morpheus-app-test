defmodule SwarmTestTest do
  use ExUnit.Case
  doctest SwarmTest

  @tag timeout: 3600_000
  test "Test" do
    # Deadlocks and inconsistent views in test_4
    SwarmTest.test(:test_4)
  end

end
