# COOLFTP

## Description

COOLFTP is a codec for the File Transfer Protocol of
[RFC 959](https://www.rfc-editor.org/rfc/rfc959). It is written in
[CLIPS](https://clipsrules.net/) and COOL, the object language of CLIPS.
The codec has two parts:

- The decoder reads a CLIPS i/o stream one octet at a time. On the
  control connection, it makes an `FTP-COMMAND` for each command line
  and an `FTP-REPLY` for each reply. On the data connection, it makes an
  `FTP-RECORD` for each record, an `FTP-PAGE` for each page and an
  `FTP-MARKER` for each restart marker.
- The encoder is the `pprint` message-handler. It writes an instance to
  a CLIPS i/o stream as FTP.

The codec decodes all of RFC 959 that is on the wire: the command
syntax, the reply codes, the command-reply sequences, the state diagrams,
the three transmission modes, the three structures, the four
representation types, the logical byte sizes and the restart markers. It
also decodes the commands and replies of RFC 2228, RFC 2389, RFC 2428
and RFC 3659.

[API.md](API.md) gives the facts and the instances that a program can
match.

## Usage

Load `program.clp` with your rules. The codec loads next to the other
COOL codecs in one environment. All of its constructs have the prefix
`ftp-` or `FTP-`.

1. Open a stream.
2. Assert the `ftp-parse` fact. Give it the stream and an id.
3. Assert the `ftp-ready` fact with the id. Run.
4. The codec makes one message and asserts `ftp-done` for it. It
   retracts `ftp-ready`.
5. Assert `ftp-ready` again for the next message, until `ftp-eof` exists.

```clips
(load program.clp)
(reset)
(open "control.ftp" input)
(assert (ftp-parse (stream input) (id input)))
(run)
(while (not (any-factp ((?e ftp-eof)) (eq ?e:id input))) do
   (assert (ftp-ready (id input)))
   (run))
(foreach ?m (ftp-messages input) (send ?m pprint t))
```

A data connection gives its `connection` and the transfer parameters
that the control connection agreed. A slot that is not given has the
default of RFC 959: ASCII non-print, file structure, stream mode.

```clips
(assert (ftp-parse (stream data) (id data) (connection data)
                   (type image) (structure record) (mode block)))
```

`program.bat` reads six control connections and one data connection from
`tests/cases`:

```
clips -f2 program.bat
```

## Test

The tests and the lint are written in CLIPS.

```
make test
```

Each file in `tests/cases` is one case. The test decodes it and writes
the result with `pprint`. The case passes if the result is the same,
octet by octet, as the file of the same name in `tests/expected`, and if
the expected file prints back unchanged. `tests/cases.clp` gives each
data case its transfer parameters and the checks of the decoded values.
The test decodes every case one time for each conflict resolution
strategy of CLIPS. All the cases decode together in one run.

```
make lint
```

The lint decodes each case one rule at a time. It makes sure that one
rule is on the agenda at a time, that all the cases decode together as
they do alone, and that no rule or function that a rule calls has `(if`
in it. It prints one line for each violation. It exits with 1 if it
finds one.

## License

Refer to [LICENSE](LICENSE).
