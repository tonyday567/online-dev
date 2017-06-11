[online](https://tonyday567.github.io/online-dev/index.html) [![Build Status](https://travis-ci.org/tonyday567/online-dev.png)](https://travis-ci.org/tonyday567/online-dev)
===

See https://tonyday567.github.io/online-dev/index.html

workflow
===

~~~
stack build && $(stack path --local-install-root)/bin/online-dev-examples run && $(stack path --local-bin)/pandoc -f markdown+lhs -t html -i examples/examples.md -o index.html --filter pandoc-include
~~~
