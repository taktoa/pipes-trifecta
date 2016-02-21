# pipes-parsers

An interface between the Parsers from the `parsers` library and those of
`pipes-parse`.

## Update to latest version of Cabal.
```sh
cabal update
cabal install cabal-install
```

## Initialize a sandbox and install the package's dependencies.
```sh
make install
```

## For Nix users:
```sh
make nix-shell
```

## Configure & build the package.
```sh
make configure
make build
```

## Test package.
```sh
make test
```

## Run executable.
```sh
make run
```

## Start REPL.
```sh
make repl
```

## Generate documentation.
```sh
make haddock
```

## Analyze coverage.
```sh
make hpc
```
