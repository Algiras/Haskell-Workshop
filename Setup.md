# Setup

Setup is based on: [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

## Installation

- Un*x operating systems:
  - `curl -sSL https://get.haskellstack.org/ | sh`
  - OR `wget -qO- https://get.haskellstack.org/ | sh`
- macOS: `brew install haskell-stack`
- Windows:
  [`Windows 64-bit Installer`](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

## Docker

- **REPL** - `run -it --rm haskell:8`

## Editor Setup

1. Download and install [Visual Studio Code](https://code.visualstudio.com/Download)
2. Install:
   - [Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)
   - [Haskell Language Server Client](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server)
     - run `stack install haskell-ide-engine` using your terminal
     - run `stack install hoogle`
     - restart `vsCode`

## Expected workflow

1. `create/open` exercise using `Visual Studio Code`
2. open a terminal in the `editor` and run `stack ghci`
3. after you update the code run `:reload`/`:r` to reload the `REPL`
4. Use integrated `Hoogle` search to find `type information` and `type class` descriptions
5. If you have tests you can run `stack test`
6. If you finished the exercise you can run `stack run`

## Troubleshooting

General reference: [Common problems](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
