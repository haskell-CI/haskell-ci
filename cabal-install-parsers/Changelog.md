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
