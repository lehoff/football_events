defmodule Exgol.Mixfile do
  use Mix.Project

  def project do
    [app: :football_events,
     version: "0.0.1",
     language: :erlang,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:gproc],
     mod: {:fe_app, []}]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    [{:gproc, git: "https://github.com/uwiger/gproc.git", branch: "master"}]
  end
end
