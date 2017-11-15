# Generates all palindroms

## Description

The input is a list of symbols, together with a maximum count.
In Haskell, it can be represented by a simple list of tuple `[(Int, a)]`.
The task is to enumerate all palindromes over the given alphabet which do not exeeding the maximum count of each symbol.
The current implementation can be found in [Palindrom.hs](./src/Palindroms.hs):

```haskell
genPalindroms :: [(Int, a)] -> [[a]]
```

## Usage

Build & run it with :

```bash
$ stack build --test --bench
```
