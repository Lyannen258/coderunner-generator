# Setup

## Install Required Tools

To build the repository, you need:
- GHC (The Glasgow Haskell Compiler)
- Stack (A build tool for Haskell)

If you are on linux, it is highly recommended to use GHCup (https://www.haskell.org/ghcup/) to install the required tools. GHCup makes it very easy to install ghc and stack and to switch between different versions.

If you are on windows, you can download the required tools from here: https://www.haskell.org/platform/

## Getting Started

When you have cloned the repository to your machine, move to the root directory and execute `stack build`.

After that, you can run the program with a template file by issuing `stack exec coderunner-generator -- _path-to-template-file_ -a _amount_of_instances_`. For the first included example, run `stack exec moodle-cpp-function -- example-files/01_writing_a_statement.tmpl -a 5`.

## Packages
This project is a multi-package-project. The __generator__ package is a library that contains the main functionality. The included packages __moodle-cpp-function__ and __moodle-cpp-program__ are executables that can be used to generate tasks for [moodle-coderunner](https://github.com/trampgeek/moodle-qtype_coderunner).