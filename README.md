![Banner](static/out.jsexe/banner.png)

[![Gitter](https://badges.gitter.im/theam/haskell-do.svg)](https://gitter.im/theam/haskell-do?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Build Status](https://travis-ci.org/theam/haskell-do.svg?branch=develop)](https://travis-ci.org/theam/haskell-do)

[haskell.do](http://haskell.do) is a Haskell code editor, centered around interactive development. You can get it on [the website](http://haskell.do).

Pull Requests are greatly appreciated, check out [our contributing guidelines](CONTRIBUTING.md).

## Building from source

The only *3rd-party* requirements to build [haskell.do](http://haskell.do) are [Stack](http://haskellstack.org/) and [NodeJS](https://nodejs.org/) (due to GHCJS).

`git clone https://github.com/theam/haskell-do && cd haskell-do`

`stack setup --stack-yaml=client-stack.yaml` to setup GHCJS (note that it isn't supported on Windows)

`stack Build.hs -h` for detailed usage of the build file.

`stack Build.hs -a` for building project.

`stack Build.hs -r` for running [haskell.do](http://haskell.do) on port `8080`.

## Contributing

Wanna contribute? Make sure that you've read our [contributor guidelines](https://github.com/theam/haskell-do/blob/master/CONTRIBUTING.md).
We'd like to hear from you and your ideas, get in touch with other contributors through:

- [Gitter](https://gitter.im/theam/haskell-do)
- [The issues page](https://github.com/theam/haskell-do/blob/master/CONTRIBUTING.md)
