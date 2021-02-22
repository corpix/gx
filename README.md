# telescheme

## development

- make sure you have `git`, `make`, [gerbil](https://cons.io)
- clone this repository
- `cd` into
- then you could want to run one of the following commands

```console
$ make test
$ make help
...
```

## environment

Environment is managed by [nix](https://nixos.org/nix) package manager.

Run it with docker:

```console
$ make run/docker/shell
...
```

Run it with [nix-cage](https://github.com/corpix/nix-cage) + `nix-shell`:

```console
$ make run/nix/cage
...
```
