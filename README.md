# Veilig
## An IDE for Haskell, notebook style

The project is composed of two sub-projects:

- The front end, called `veilig-gui`
- The back end, called `veilig-core`

## Requirements

- NodeJS v6.8.1 or superior
- Stack v1.2.0 or superior

## Building

For compiling the **backend**, execute in the `veilig-core/` directory:

- `stack build` - This will download the necessary dependencies and build the project.

For compiling the **frontend**, execute in the `veilig-gui/` directory:

- `npm install -g bower pulp purescript` - This will install:
  - **Bower** - A dependency tool used for purescript libraries
  - **Pulp** - A build tool for purescript
  - **Purescript** itself
- `npm install` - Installs necessary dependencies for the desktop app
- `bower install` - Installs purescript libraries used for the project
- `npm run build` - Build the project

Alternatively, for the last step, one can use `npm run watch` to make `pulp`
watch for source code changes, building the frontend each time the code changes.

## Running

After building the project, to run it one must run, in the following order:

1. In the backend folder, `stack exec veilig-core-exe`
2. In the frontend folder, `npm run start`
