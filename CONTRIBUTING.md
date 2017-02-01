# Contributing to HaskellDO

As of right now, we're just releasing something we think is good enough to get
going with, but the community is who really has the ability to bring an idea like
this into focus and build it into something amazing. Though there are a few guidelines we'd like our contributors to follow so that we can keep on top of things and streamline the PR process.

# HaskellDO Core vs GUI
Right now HaskellDO is separated into two modules. The `core` module holds the backend and engine of HaskellDO. `core` is written in Haskell and focuses primarily on interacting with notebooks received from the frontend. Anything that directly impacts a notebook or project, say adding `git` commands to HaskellDO, should be done in `core`.
The `gui` module holds the frontend of HaskellDO. Right now it is written in PureScript, though we are thinking about converting it all to Haskell at a later date. Any GUI changes, QOL improvements, etc, should be changed in `gui`.

# Getting Started
* Create a [GitHub](https://github.com) account if you do not already have one
* Check if a ticket for your issue exists, if not create one
	* Make sure your ticket details the issue and the steps to reproduce the bug
	* If your ticket proposes a new feature for HaskellDO, please provide an explanation of the feature, what problem is solves, and possible use cases
* Fork the repository on GitHub

# Changing HaskellDO
* Create a branch to work on your feature with a descriptive name
	* Generally branched off of `master`
* Make commits frequently with descriptive comments (detailed below)
* Add tests to ensure proper functionality
* Please do not submit until all tests are passed

Commit messages should stick to the following format: (issue number) issue name description

E.g:

```
(HD-01) Example issue
Steps to recreate: etc

An issue would then here go into detail describing the issue, and perhaps even suggesting a fix
```

# Making Small Changes
When changing things like documentation, it is not always necessary to create a ticket. Instead simply add the documentation, and send a PR with a message of the following form:
```
(doc) Added documentation to <file-name>
<file-name> lacked proper documentation on how <function> works.
This commit adds documentation describing <function>, and provides various examples.
```

# Submitting Changes
* Push your changes to the branch in your fork of the repository
* Submit a pull request
* The HaskellDO team will review your PR as quickly and provide feedback
* After receiving feedback, either make the required changes, or your branch will be merged

Thanks for contributing to HaskellDO, happy hacking!
