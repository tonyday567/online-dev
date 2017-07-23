[online-market](https://tonyday567.github.io/online-market/index.html)
===

[![Build Status](https://travis-ci.org/tonyday567/online-market.svg)](https://travis-ci.org/tonyday567/online-market)

See https://tonyday567.github.io/online-market/index.html

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/online-market-examples run" --exec "$(stack path --local-bin)/pandoc -f markdown -i examples/examples.md -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
