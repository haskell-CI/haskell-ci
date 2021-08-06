## 0.4.3

- Use `Cabal-3.6`

## 0.4.2

- `findConfig` respects `CABAL_DIR` environment variable

## 0.4.1

- Use `Cabal-3.4`

## 0.4

- Rewrite `Cabal.Index` module
  - Added `riTarOffset` field to `ReleaseInfo`
  - Make `SHA256` be backed by four `Word64`.
    Hackage cache file size drops from 11673149 to 4423787 bytes.

- Update dependencies

## 0.3

- Require `Cabal-3.2`.
- Rename `prjOrigFields` to `prjOtherFields` to reflect its intended purpose:
  contain all fields of a `cabal.project` file that are not already covered by
  a different field of `Project`, such as `prjPackages`, `prjOptPackages`,
  `prjSourceRepos`, etc. The semantics of `parseProject` have also been changed
  accordingly, so `Project`s produced by `parseProject` will no longer have
  `prjOtherFields` entries that overlap with other `Project` fields.
- `ParseError` from `Cabal.Parse` now parameterizes the list type used in
  `peErrors`. Most of the time, this will be instantiated to `NonEmpty`.
- Add `NFData` instances

## 0.2

Add `repoSecure` field to `Repo` in `Cabal.Config`.
