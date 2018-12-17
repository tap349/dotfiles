# https://hexdocs.pm/elixir/master/Code.html#format_string!/2-options
[
  # these packages have adopted .formatter.exs
  import_deps: [:phoenix, :ecto, :plug],
  inputs: ["mix.exs", "{config,lib,test}/**/*.{ex,exs}"],
  line_length: 80,
  locals_without_parens: [defenum: 2]
]
