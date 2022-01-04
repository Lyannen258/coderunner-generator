# Setup

## Install Required Tools
To build the repository, you need two things:
- GHC (The Glasgow Haskell Compiler)
- Stack (A build tool for Haskell)

Additionally, it is recommended to install the Haskell Language Server (HLS). If you use Visual Studio Code, HLS is contained in the Haskell extension and you do not need to download it manually.

If you are on linux, it is highly recommended to use GHCup (https://www.haskell.org/ghcup/) to install the required tools. GHCup makes it very easy to install ghc and stack and to switch betweend different versions.

If you are on windows, you can download the required tools from here: https://www.haskell.org/platform/

## Getting Started

If you cloned the repository to your machine, move to the root directory and execute `stack build`.

After that, you can run the program with an template file by issuing `stack exec coderunner-generator _path-to-template-file_`. For the first included example, run `stack exec coderunner-generator example-files/01_writing_a_statement.txt`.

To generate the documentation, run `stack haddock`. You can then open it in the browser by using `stack haddock --open coderunner-generator`