# stack-tpls

Collect all [Haskell Stack](https://docs.haskellstack.org) template files.

But, I implemented to collect from only GitHub yet.

## Requirement

## Install

```
$ git clone https://github.com/matsubara0507/stack-tpls.git
$ stack install
```

## Usage

Pease set GitHub Personal Token (can use `.env`, `~/.env`):

```
GH_TOKEN=xxx
```

show all stack-templates (in GitHub):

```
$ stack-tpls --list
github:commercialhaskell/chrisdone.hsfiles
github:commercialhaskell/foundation.hsfiles
github:commercialhaskell/franklinchen.hsfiles
github:commercialhaskell/ghcjs-old-base.hsfiles
 .
 .
 .
```

this list is saved local cache.
if update local cache, exec `stack-tpls --list --update`.

show any hsfiles:

```
$ stack-tpls github:commercialhaskell/rio.hsfiles
{-# START_FILE .gitignore #-}
*.cabal
*~
 .
 .
 .
```

or show any link of hsfiles:

```
$ stack-tpls --link github:commercialhaskell/rio.hsfiles
https://github.com/commercialhaskell/stack-templates/blob/master/rio.hsfiles
```
