# Changelog for stack-tpls

## 2.0.0

- Misc: update LTS to 15.0 (GHC 8.8)
- Misc: update deps packages
  - extensible to 0.7.1
  - megaparsec to 8.0
  - dotenv to 0.8
  - use githash instead of gitrev
- Refactor: use mix.hs
- CI: add GitHub Actions config
- Refactor: with HLint v2.2.11
- Fix: use `(</>)` to construct cache path
- Fix: create cache dir when get cache path
- Fix: type of GitHub API response field `endCursor` to nullable

## Unreleased changes
