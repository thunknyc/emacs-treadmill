# Treadmill: Structured Network Interaction with a Gerbil REPL

## Status

It's early days. There are no modes. No major mode for the interaction
buffer, no minor mode for evaluating Gerbil code from within a source
file. There's not even an interactive way to spawn a net REPL.

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
