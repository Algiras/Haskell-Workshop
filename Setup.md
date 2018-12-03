# Setup

Setup is based on: [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

## Installation

- Un*x operating systems:
  `curl -sSL https://get.haskellstack.org/ | sh` OR `wget -qO- https://get.haskellstack.org/ | sh`
- Windows:
  [`Windows 64-bit Installer`](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

## Docker

- **REPL** - `run -it --rm haskell:8`

## Editor Setup

1. Download and install [Visual Studio Code](https://code.visualstudio.com/Download)
2. Install:
   - [Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)
   - [Haskero](https://marketplace.visualstudio.com/items?itemName=Vans.haskero)
    - run `stack install intero` using your terminal
    - restart `VsCode`

## Troubleshooting

General reference: [Common problems](https://docs.haskellstack.org/en/stable/install_and_upgrade/)