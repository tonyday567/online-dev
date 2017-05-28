[online](https://tonyday567.github.io/online-dev/index.html) [![Build Status](https://travis-ci.org/tonyday567/online-dev.png)](https://travis-ci.org/tonyday567/online-dev)
===

See https://tonyday567.github.io/online-dev/index.html

workflow
===

    stack build --copy-bins --exec "online-dev-examples run" --exec "pandoc -f markdown+lhs -t html -i examples/examples.md -o index.html --filter pandoc-include"
