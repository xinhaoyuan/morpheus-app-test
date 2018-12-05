defmodule SwarmTestSimpleTest do
  use ExUnit.Case
  doctest SwarmTestSimple

  @tag timeout: 3600_000
  test "Test" do
    SwarmTestSimple.test()
  end

end
