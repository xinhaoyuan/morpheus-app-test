defmodule SwarmTest.MixProject do
  use Mix.Project

  def project do
    [
      app: :swarm_test,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:morpheus, git: "https://github.com/xinhaoyuan/morpheus.git", runtime: false},
      {:firedrill, git: "https://github.com/xinhaoyuan/firedrill.git", override: true, runtime: false},
      {:swarm, git: "https://github.com/bitwalker/swarm.git", ref: "738decbc9cfcc4b0de80b6a4f72b092b01f4c8f7", runtime: false}
    ]
  end
end
