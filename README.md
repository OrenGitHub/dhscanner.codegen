# dhscanner.codegen

An [AST][1]-based type resolver
See also [OrenGitHub/dhscanner.ast][1]

## run on docker

```bash
docker build --tag host.codegen --file Dockerfile .
docker run -p 8002:3000 -d -t --name codegen host.codegen
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