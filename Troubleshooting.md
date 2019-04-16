# Troubleshooting

This file describes how to handle any issues that might happen during workshop and how to address them.

- When I start `VSCode` I get an error about `hie` using incorrect version?
  - In Haskell setting you can edit `Language Server Haskell: Hie Executable Path`. Try setting it `hie` version. You can find installed versions under `/Users/{YOUR_USERNAME}/.local/bin` it might look like `hie-8.4.4`.

- It takes for ever to build all `hie` versions, I want to actually work on Haskell in this workshop!
  - Relax and install just a single version(`hie-8.4.4`)
  
```bash
stack ./install.hs hie-8.4.4
stack ./install.hs build-doc-8.4.4
```