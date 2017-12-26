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


## Docker-related

`haskell-do` can be executed within a Docker container. For convenience, the building and execution commands are provided within the Makefile.

NB : These instructions assume the user has already created a docker machine called "dev".

NB 2 : OSX users usually need to run the following command to configure VirtualBox in order to set up port forwarding:

    VBoxManage modifyvm "dev" --natpf1 "tcp-port8080,tcp,,8080,,8080"

Once that is done, Docker can be configured and started:

    docker-machine start dev

    eval $(docker-machine env dev)

Then, the `haskell-do` image can be built and run:

    make docker

    make docker-run

If everything went well, it is now possible to point a browser to `http://localhost:8080` and work with `haskell-do`.



## Contributing

Would you like to contribute? Make sure that you've read our [contributor guidelines](https://github.com/theam/haskell-do/blob/master/CONTRIBUTING.md).
We'd like to hear from you and your ideas, get in touch with other contributors through:

- [Gitter](https://gitter.im/theam/haskell-do)
- [The issues page](https://github.com/theam/haskell-do/blob/master/CONTRIBUTING.md)
