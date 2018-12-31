# Treadmill: Gerbil-Emacs Networked REPL Environment

![Gerbil Screenshot](/doc/screenshot.png?raw=true "Gerbil in action, New Year's Eve, 2018")

## Introduction

[Gerbil](https://cons.io) is a great [Scheme](https://schemers.org)
and it deserves a great development environment. While you're waiting
for someone to create it, you can use Treadmill.

Treadmill is shamelessly modelled on
[CIDER](https://cider.readthedocs.io/en/latest/), the Clojure
Emacs-based IDE. I wrote a profiler for CIDER -- which later became
_the_ profiler for it -- and while doing so I came to appreciate its
design, as well as the leadership of the team.

Anyway, while Treadmill does not currently support the sort of
modularity that to me is the hallmark of modern CIDER's elegance,
that's the direction it's moving in.

(If you're wondering what exactly I mean about CIDER's modularity,
here's a quick background: If you're writing an IDE in Emacs, you
quickly realize that you'll be writing both Elisp and code for your
REPL back-end, and what you _don't_ want to do is require a lot of
tedious, error-prone configuration to ensure that the Elisp is talking
to a REPL that has the appropriate support for the IDE's features. In
Gerbil, I think we'll handle this by associating packages with IDE
modules and then installing them and initializing them automatically.)

## Installation

I'm currently going through the process of getting Treadmill available
through MELPA; until that's done you won't be able to do the regular
`package-list-packages` routine; you'll need to grab `treadmill.el`
and put it in your Emacs load path. After you do that, stick this in
your `.emacs.d/init.el`, `.emacs`, or whatever:

```
(require 'treadmill) ; will not be requires with MELPA package
(add-hook 'gerbil-mode-hook #'treadmill-gerbil-mode)

```

Additionally, you will need to [install
Gerbil](https://cons.io/guide/) and the Treadmill server package:

`gxpkg install github.com/thunknyc/gerbil-treadmill`

Treadmill finds `gxi`, the Gerbil interpreter, using the `GERBIL_HOME`
environment variable. You can override this behavior by setting
`treadmill-interpreter-name`.

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

Once you're in the `*treadmill*` buffer, you can type expressions and
evaluate them in the current module.

## Default Key bindings

### Interaction buffers

| Binding | Description |
| --- | --- |
| `RET` | Evaluate expression entered at prompt. |
| ` C-c q` | Close interactive session and associated processes and buffers. |
| `C-c m` | Prompt for a module name in which to evaluate entered expressions. |
| `C-c C-z` | Switch back to the most recent switched-from Gerbil buffer. |

### Gerbil buffers

| Binding | Description |
| --- | --- |
| `C-x C-e` | Evaluate last sexp in current Treadmill interaction buffer session using this buffer's current module. |
| `C-c C-e` | Evaluate the current region. |
| `C-c C-c`, `C-M-x` | Evaluate the current toplevel. |
| `C-c m` | Change the buffer's current module, overriding the module discerned from the nearest `gerbil.pkg` file. |
| `C-c C-z` | Switch to the current Treadmill interaction buffer. |

## Notes

It's early days.

## Buffers

There are potentially three buffers associated with a Treadmill session:

1. A `*treadmill-spawn*` buffer, in which gxi is run and a the net
REPL is launched. If you set up a net REPL inside your app by hand
using thunknyc/treadmill, you can connect to it via
`treadmill-connect`, and this buffer won't exist. No user serviceable
parts inside.

2. A `*treadmill-repl*` buffer, the buffer for the connection to the
network REPL, in which Treadmill evaluates code. Again, no user
serviceable parts inside.

3. A `*treadmill*` buffer, the buffer in which human interaction takes
place.

## License

MIT.
