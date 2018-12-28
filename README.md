# Treadmill: Structured Network Interaction with a Gerbil REPL

## Installation

Put `treadmill.el` somehwere that Emacs knows about and stick this in your `.emacs.d/init.el`, `.emacs`, etc.:

```
(require 'treadmill-mode)
(setq treadmill-interpreter-path "/usr/local/wherever-gerbil-lives")
```

Additionally, you will need to install the Gerbil side of Treadmill:

`gxpkg install github.com/thunknyc/gerbil-treadmill`

## Usage

`M-x treadmill-spawn`

> Run a local Gerbil interpreter and start up a network REPL. Then,
  connect to the REPL.

`M-x treadmill-connect`

> Prompt you for a host and a port and will then connect to a network
  REPL. Presumes that you've somehow gotten a network REPL running
  inside your program without Treadmill's help.

`M-x treadmill-quit`

> Destroy the current Treadmill interaction buffer, which by the way
  needs to be the current buffer.

## Notes

It's early days. There's no minor mode for evaluating Gerbil code from
within a source file. I haven't really figured out how to charmingly
ask for standard input or show standard output. Everything's really
ugly.

## Buffers

There are potentially three buffers associated with a Treadmill session:

1. A *treadmill-spawn* buffer, in which gxi is run and a the net REPL
is launched. If you set up a net REPL inside your app by hand using
thunknyc/treadmill, you can connect to it via `treadmill-connect`, and
this buffer won't exist. No user serviceable parts inside.

2. A *treadmill-repl* buffer, the buffer for the connection to the
network REPL, in which Treadmill evaluates code. Again, no user
serviceable parts inside.

3. A *treadmill-interaction* buffer, the buffer in which human
interaction takes place.

## License

MIT.
