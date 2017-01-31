# HaskellDO

HaskellDO is a Haskell code editor,  centered around  interactive development.
This is a **pre-alpha**  version,  not expected to be used in production. As a
prototype, major changes will be applied in the future and also major features
will be implemented.

The current version is written in [PureScript](http://www.purescript.org/) and
[Haskell](https://www.haskell.org/). Although  the former might  change in the
future.


## Downloads
---
The only *3rd-party* requirement to run HaskellDO is [Stack](http://haskellstack.org/)

You can find a binary release according to your operating system in the
[releases page](https://github.com/theam/haskelldo/releases).

## Building from source
---
### Requirements

- NodeJS v6.8.1 or superior
- Stack v1.2.0 or superior

Begin by cloning this repository.

The project is composed of two sub-projects:

- A PureScript front end, residing in `gui`
- A Haskell back end, residing in `core`

### Compilation
For compiling the **backend**, execute in the `core/` directory:

- `stack build` - This will download the necessary dependencies and build the project.

For compiling the **frontend**, execute in the `gui/` directory:

- `npm install -g bower pulp purescript` - This will install:
    - **Bower** - A dependency tool used for purescript libraries
    - **Pulp** - A build tool for purescript
    - **Purescript** itself

- `npm install` - Installs necessary dependencies for the desktop app
- `bower install` - Installs purescript libraries used for the project
- `npm run build` - Build the project

Alternatively, for the last step, one can use `npm run watch` to make `pulp`
watch for source code changes, building the frontend each time the code changes.

### Running in dev-mode

After building the project, run, in the following order:

1. In the backend folder, `stack exec haskelldo-core`
2. In the frontend folder, `npm run start dev`, to make the GUI connect to the running
   dev server.