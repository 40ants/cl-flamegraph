(defsystem "flamegraph"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "BSD"
  :class :package-inferred-system
  :description "Flamegraphs are a cool way to search for hotspots in your code. This system uses SBCL's statistical profiler, to generate FlameGraph charts."
  :depends-on ("flamegraph/core"))
