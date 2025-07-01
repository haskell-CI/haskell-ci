## 0.6.4

- Add support for reading project files with conditionals.

## 0.6.3

- Drop support for GHC prior 8.8.4
- Use `Cabal-syntax-3.14`

## 0.6.2

- Drop support for GHC prior 8.6.5
- Use `Cabal-syntax-3.12`

## 0.6.1

- Use `Cabal-syntax-3.10`

## 0.6

- Add sizes of tarball and cabal files to `Cabal.Index.ReleaseInfo` data structure.

## 0.5

- Move to use `Cabal-syntax` package
  It's possible to have a build-plan with (old) `Cabal`,
  and new `cabal-install` syntax, which may cause
  `...Version` is not `...Version` like errors.

## 0.4.5

- Resolve `.tar.gz` package files to `file:` URI-locations

## 0.4.4

- Try to fix glob behavior on Windows
- Add `Show (Project ...)` instance

## 0.4.3

- Use `Cabal-3.6`
- `resolveConfig` respects `CABAL_DIR` environment variable

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
