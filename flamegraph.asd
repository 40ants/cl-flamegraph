(defsystem "flamegraph"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "BSD-3-Clause"
  :description "Flamegraphs are a cool way to search for hotspots in your code. This system uses SBCL's statistical profiler, to generate FlameGraph charts."
  :depends-on (:alexandria :sb-sprof)
  :components ((:module "src" :components ((:file "core")))))
