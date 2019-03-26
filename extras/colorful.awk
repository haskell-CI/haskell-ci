function blue(s) { printf "\033[0;34m" s "\033[0m " }
BEGIN { state = "output"; }
/^-----BEGIN CABAL OUTPUT-----$/ { state = "cabal" }
/^-----END CABAL OUTPUT-----$/ { state = "output" }
!/^(-----BEGIN CABAL OUTPUT-----|-----END CABAL OUTPUT-----)/ {
  if (state == "cabal") {
    print blue($0)
  } else {
    print $0
  }
}
