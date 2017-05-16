# Haskell.do

[![Gitter](https://badges.gitter.im/theam/haskell-do.svg)](https://gitter.im/theam/haskell-do?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Haskell.do is a Haskell code editor, centered around interactive development.

This is a **pre-release** version, **not expected to be used in production**. As a
prototype, major changes may be applied in the future that could break backwards
compatibility. Pull Requests will be greatly appreciated, check out [our contributing guidelines](CONTRIBUTING.md).

The current version is written in [Haskell](https://www.haskell.org/) and
[PureScript](http://www.purescript.org/), but on next releases we're aiming for
a pure Haskell implementation.

## Usage

The only *3rd-party* requirement to run Haskell.do is [Stack](http://haskellstack.org/) and [NodeJS](https://nodejs.org/).

Before proceeding, run a `npm install -g purescript@0.10.7 pulp@10.0.3 bower` to install the required NodeJS binaries.

`git clone https://github.com/theam/haskell-do && cd haskell-do`

`stack Build.hs -h` for detailed usage of the build file.

`stack Build.hs -d` for installing the required dependencies, and

`stack Build.hs -a` for building the whole Haskell.do project.

### Initializing a project
Begin by creating a **new** Stack project in another terminal:

`stack new your-project-name simple-library`. Be sure to use _hyphens_ not underscores.

After doing that, `cd your-project-name && stack setup && stack build`.

Back in the terminal where you cloned Haskell.do, run `stack Build.hs -r`,
Haskell.do will open and it will ask you to open a Stack project.

Navigate to the root of the project you just created and open `src/Lib.hs`

> Due to a bug, the first time that Haskell.do opens, it won't show the code, and the backend won't spawn. It is recommended that you restart Haskell.do. It should run fine when you open it the second time.

### Main interface
Let's begin by adding a text cell for documenting our analysis:

![Imgur](http://i.imgur.com/QAVI2WC.gif)

Haskell.do's text editor is based on [SimpleMDE](https://simplemde.com/) for
handling the editing and rendering of the "documentation" part of our code.

It supports all the features that you would expect from a normal markdown
editor, including image embedding.

- Do a single click out of the editor to render the current text
- Do a double click inside of the editor to come back to the text editing
  view.

![Imgur](http://i.imgur.com/ElGTVLK.gif)

Now, **it's time to work with some code, let's insert a code cell**.
In it, we write regular Haskell code, like we would do in a normal Haskell
module.

In fact, our whole page is just a Haskell file that can be used in any
Haskell, project. No need to export/import!

![Imgur](http://i.imgur.com/8jVxh6A.gif)

Now we have to try our new algorithm we spent hours researching on.
No, we do not have to spawn a `stack ghci` or a `stack repl`. Haskell.do
manages that for us and reloads the code each time we ask it to evaluate
some expression.

Just click on the **toggle console** button and press the save button to
enable it.

After writing your expression, press return [twice](https://github.com/theam/haskell-do/issues/1)
to get the result written on screen.

![Imgur](http://i.imgur.com/jgZQAvu.gif)

But, what does our *real* module file look like? Let's see the contents
of our `Main.hs` file:

```haskell
-- # Analyzing dog cuteness with genetic algorithms
--
-- After going **through thorough and tough thoughts**, we decided to use a simple example.

a = [1..20]
b = f <$> a

f :: Int -> Int
f x = x * 3 + 4
```

Just regular Haskell! Ready for deployment!

## Contributing

Wanna contribute? Make sure that you've read our [contributor guidelines](https://github.com/theam/haskell-do/blob/master/CONTRIBUTING.md).
We'd like to hear from you and your ideas, get in touch with other contributors through:

- [Gitter](https://gitter.im/theam/haskell-do)
- [The issues page](https://github.com/theam/haskell-do/blob/master/CONTRIBUTING.md)
