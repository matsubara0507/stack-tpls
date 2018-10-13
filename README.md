# stack-template-collector

Collect all Haskell Stack template files.

I implemented to collect from only GitHub yet.

## Requirement

## Install

```
$ git clone https://github.com/matsubara0507/stack-template-collector.git
$ stack install
```

## Usage

Pease set GitHub Personal Token:

```
GH_TOKEN=xxx
```

show all stack-templates (in GitHub):

```
$ stack-template-collector
github:commercialhaskell/chrisdone.hsfiles
github:commercialhaskell/foundation.hsfiles
github:commercialhaskell/franklinchen.hsfiles
github:commercialhaskell/ghcjs-old-base.hsfiles
 .
 .
 .
```

show any hsfiles:

```
$ stack-template-collector show github:commercialhaskell/rio.hsfiles
{-# START_FILE .gitignore #-}
*.cabal
*~
 .
 .
 .
```
