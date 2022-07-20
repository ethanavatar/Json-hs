# Json-hs

A simple JSON parser written in Haskell.

Inspired by [this video](https://www.youtube.com/watch?v=N9RUqGYuGfw) ([source](https://github.com/tsoding/haskell-json))

I don't pretend to understand category theory. This was very difficult.

## Usage

I didn't make any sort of interface for it yet, so the only way to use it is through GHCI.

```bash
$ ghci
ghci> :l parser
ghci> parseFile "./test.json" parseValue
```