## Setup

Enter the development environment:

```bash
nix develop

# or, if GHC gets OOM-killed
nix develop --cores 2 --max-jobs 1
```


Create a `.session` file with your AoC session cookie for automatic input downloads.

## Running

```bash
cabal run aoc2025
```

## AI disclaimer

Claude has been used for initial project setup, learning about Haskell, linting and making build or structural changes.

However, thinking about the puzzles themselves (and writing code for solving the puzzles) was done entirely manually.

