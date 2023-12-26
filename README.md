# dhscanner.typer

An [AST][1]-based type resolver
See also [OrenGitHub/dhscanner.ast][1]

## run on docker

```bash
docker build --tag host.parser --file Dockerfile .
docker run -p 8001:3000 -d -t --name parser host
```

## run

```bash
# from dhscanner
python analyze.py
```

## documentation

```
cabal haddock --html $(find src -name \*.hs)
```

[1]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
[2]: https://github.com/OrenGitHub/dhscanner.ast