# COOLFTP API

This document gives the facts, the instances and the functions of the
codec. A program matches them with a pattern, or finds them with
`find-instance` or `find-fact`.

Each instance has an `id` slot. The `id` is the id of the stream that the
message came from. Each instance has a `sequence` slot. It numbers the
messages of a stream from 1, in the order the codec read them. The codec
makes the instance when the message is complete. Then it asserts
`ftp-done` for the instance.

Each class has a `pprint` message-handler. It writes the instance to a
logical name. The handlers that query an instance have no state. A reply
does not know the command before it. A program that checks a dialogue
keeps the command and asks the reply.

## Facts

### `ftp-parse`

The start fact. Assert it one time for each stream. The codec retracts it
and asserts `ftp-stream`.

| Slot | Value |
|---|---|
| `stream` | The logical name of an open file or socket. |
| `id` | The id of the results from this stream. |
| `connection` | `control` or `data`. The default is `control`. |
| `type` | The representation type of a data connection: `ascii`, `ebcdic`, `image` or `local`. The default is `ascii`. |
| `form` | The form of an `ascii` or `ebcdic` type: `non-print`, `telnet` or `carriage-control`. The default is `non-print`. |
| `byte-size` | The bits in a logical byte, the `n` of `TYPE L n`, 1 to 56. The default is 8. |
| `structure` | `file`, `record` or `page`. The default is `file`. |
| `mode` | `stream`, `block` or `compressed`. The default is `stream`. |

The transfer parameters are for a data connection. They are the values
that the control connection agreed with `TYPE`, `STRU` and `MODE`. The
defaults are those of RFC 959 5.1. A control connection does not use
them.

### `ftp-ready`

The codec reads a stream only while this fact exists for its id. The
codec retracts the fact when a message is complete. Assert it again to
read the next message. A server can poll between messages.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |

### `ftp-stream`

The codec asserts one for each stream. Match it to get the logical name
of the stream for the id, and the transfer parameters.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `stream` | The logical name of the stream. |
| `connection` | `control` or `data`. |
| `type` | The representation type. |
| `form` | The form. |
| `byte-size` | The logical byte size. |
| `structure` | The structure. |
| `mode` | The mode. |

### `ftp-done`

The codec asserts one for each complete message. The program that
matches it must retract it.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `message` | The instance: an `FTP-COMMAND`, `FTP-REPLY`, `FTP-RECORD`, `FTP-PAGE` or `FTP-MARKER`. |

### `ftp-eof`

The end fact. The codec asserts it at the next `ftp-ready` fact after the
stream ends. The program that matches it retracts it.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |

On the control connection, a last line without LF is not a message. On
the data connection, the close of the connection is the end of file only
in stream mode with file structure (RFC 959 3.4.1). The codec makes that
record first. An empty stream is one empty record. In all other
conditions, the codec asserts `ftp-eof` and changes no other fact. The
parts of the cut message stay in the parser. The program decides what
occurs then.

### Internal facts

The codec keeps its state in these facts. A program does not match them.

| Fact | Value |
|---|---|
| `ftp-parser` | The parser state of one stream. Its `name` is the id. It keeps the text, the code digits and the lines of the message in progress, the transfer parameters, and the block, page and bit state of a data connection. |
| `(ftp-char <id> <code>)` | The octet read, or -1 at the end of the stream. |
| `(ftp-byte <id> <byte> <kind>)` | A transfer byte of a data connection. `kind` is `data` if a transfer byte is a logical byte. It is `transport` if the codec unpacks logical bytes from transfer bytes. |
| `(ftp-signal <id> <descriptor>)` | A descriptor code that ends a record: 128 EOR, 64 EOF. |

## Functions

### `(ftp-messages ?id)`

The messages of a stream, in the order of their `sequence`. It stops at
the first number with no instance. If you unmake a message, it does not
return the messages after it.

The other `ftp-` deffunctions are helpers of the rules and handlers.

## Global variables

The tables that the handlers read. A program does not set them.

| Variable | Value |
|---|---|
| `?*ftp-verbs-959*` | The verbs of RFC 959. |
| `?*ftp-verbs-2228*` | `AUTH ADAT PROT PBSZ CCC MIC CONF ENC` (RFC 2228). |
| `?*ftp-verbs-2389*` | `FEAT OPTS` (RFC 2389). |
| `?*ftp-verbs-2428*` | `EPRT EPSV` (RFC 2428). |
| `?*ftp-verbs-3659*` | `MLST MLSD SIZE MDTM` (RFC 3659). |
| `?*ftp-ascii*` | The ASCII characters of codes 1 to 127, in order. The encoder uses it to make the bytes of a string. |

## Instances

### `FTP-MESSAGE`

The parent of each message. A program does not make one.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `sequence` | The position of the message in the stream, from 1. 0 in an instance that a program makes. |

### `FTP-COMMAND`

One command line of the control connection: a verb, one space and an
argument, then CR LF. The decoder drops the Telnet commands of RFC 854,
for example the `IAC IP` and `IAC DM` before `ABOR`. It drops blank lines
and leading spaces.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `sequence` | The position in the stream. |
| `verb` | The verb as an upper case symbol: `USER`, `PASS`, `RETR`. The default is `NOOP`. |
| `argument` | The text after the first space, or `""`. |

Message-handlers:

| Message-handler | Result |
|---|---|
| `(send ?c pprint ?t)` | Writes the verb, then a space and the argument if it is not `""`, then CR LF. |
| `(send ?c standard)` | The RFC that defines the verb: 959, 2228, 2389, 2428 or 3659. `FALSE` for an unknown verb. |
| `(send ?c group)` | The group of RFC 959 4.1: `access-control`, `transfer-parameter`, `service` or `unknown`. The RFC 2228 verbs are access control. `EPRT` and `EPSV` are transfer parameters. The other extension verbs are service. |
| `(send ?c valid)` | `TRUE` if the argument has the syntax of RFC 959 5.3, or of the extension RFC, for the verb. `FALSE` for an unknown verb. Refer to the table below. |
| `(send ?c minimum)` | `TRUE` for a command of the minimum implementation of RFC 959 5.1: `USER`, `QUIT`, `PORT`, `RETR`, `STOR`, `NOOP`, `TYPE A` or `TYPE A N`, `MODE S`, `STRU F` or `STRU R`. |
| `(send ?c pathname)` | The argument of `CWD`, `SMNT`, `RETR`, `STOR`, `APPE`, `RNFR`, `RNTO`, `DELE`, `RMD`, `MKD`, `LIST`, `NLST`, `STAT`, `MLST`, `MLSD`, `SIZE` or `MDTM` if it is not `""`. If not, `FALSE`. |
| `(send ?c type-code)` | The representation type of a `TYPE` command, as a multifield: `ascii` or `ebcdic` and its form, `non-print`, `telnet` or `carriage-control`; `image`; or `local` and its byte size. Empty for another verb or an argument that RFC 959 does not permit. |
| `(send ?c structure-code)` | The structure of a `STRU` command: `file`, `record` or `page`. If not, `FALSE`. |
| `(send ?c mode-code)` | The mode of a `MODE` command: `stream`, `block` or `compressed`. If not, `FALSE`. |
| `(send ?c allocation)` | The size of an `ALLO` command, and the maximum record or page size if `R` gives one: `ALLO 1024 R 128` is `(1024 128)`. Empty for another verb or a bad argument. |
| `(send ?c marker)` | The argument of a `REST` command. If not, `FALSE`. |
| `(send ?c host-port)` | The host, as a string, and the port, as an integer, of a `PORT` or `EPRT` command. `EPRT` keeps the host as sent, IPv4 or IPv6. Empty for another verb or a bad argument. |
| `(send ?c replies)` | The reply codes that the verb can get, from the command-reply sequences of RFC 959 5.4 and the extension RFCs. Empty for an unknown verb. |
| `(send ?c next)` | The verbs that follow a `3yz` reply to the command, from the state diagrams of RFC 959 6: `PASS` after `USER`, `ACCT` after `PASS`, `RNTO` after `RNFR`, `RETR STOR APPE` after `REST`. Empty for the other verbs. |

The syntax that `valid` checks, by verb:

| Verbs | Valid argument |
|---|---|
| `CDUP QUIT REIN PASV STOU ABOR PWD SYST NOOP FEAT CCC` | `""` |
| `USER PASS ACCT CWD SMNT RETR STOR APPE RNFR RNTO DELE RMD MKD SITE OPTS SIZE MDTM AUTH ADAT MIC CONF ENC` | Not `""` |
| `LIST NLST STAT HELP MLST MLSD EPSV` | Each argument, or `""` |
| `PORT` | Six decimal numbers, 0 to 255, with commas |
| `TYPE`, `STRU`, `MODE`, `ALLO` | What `type-code`, `structure-code`, `mode-code` or `allocation` decodes |
| `REST` | Printable characters, not `""` |
| `EPRT` | `<d><proto><d><host><d><port><d>`, as `host-port` decodes |
| `PBSZ` | Decimal digits |
| `PROT` | `C`, `S`, `E` or `P` |

### `FTP-REPLY`

One reply of the control connection: a three digit code and its text. A
multi-line reply, `xyz-` to `xyz `, keeps the lines before the last in
`lines`. The first is the text after `xyz-`. The last line is `text`.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `sequence` | The position in the stream. |
| `code` | The code as an integer. The default is 200. |
| `lines` | The lines before the last, as strings, without their `xyz-`. Empty for a one-line reply. |
| `text` | The last line after the code and the space, or `""`. |

Message-handlers:

| Message-handler | Result |
|---|---|
| `(send ?r pprint ?t)` | Writes `xyz-` and the first of `lines`, then each other line, then `xyz ` and `text`. Each line ends with CR LF. A reply with no `lines` is one line. |
| `(send ?r category)` | The first digit (RFC 959 4.2, RFC 2228): `positive-preliminary`, `positive-completion`, `positive-intermediate`, `transient-negative-completion`, `permanent-negative-completion`, `protected` for `6yz`, or `unknown`. |
| `(send ?r group)` | The second digit (RFC 959 4.2): `syntax`, `information`, `connections`, `authentication`, `unspecified`, `file-system` or `unknown`. |
| `(send ?r positive)` | `TRUE` for a `1yz`, `2yz` or `3yz` reply. |
| `(send ?r meaning)` | The text that RFC 959 4.2.2, RFC 2228 or RFC 2428 gives the code, as a string. If not, `FALSE`. |
| `(send ?r standard)` | The RFC that defines the code: 959, 2228 or 2428. `FALSE` for an unknown code. |
| `(send ?r answers ?verb)` | `TRUE` if the code is a reply that the verb can get (RFC 959 5.4). Give the verb as a symbol, or `CONNECTION` for the greeting. |
| `(send ?r outcome ?verb)` | What the reply means for the verb, by the state diagrams of RFC 959 6: `success`, `failure`, `error`, `wait`, `continue` or `unknown`. Refer to the table below. |
| `(send ?r full-text)` | All lines of the reply, with a line break between each, and `text` last. |
| `(send ?r pathname)` | The quoted pathname of a `257` reply. Each doubled quote is one quote. `FALSE` if `text` has no quote. |
| `(send ?r features)` | The middle lines without their leading spaces: the features of a `211` reply to `FEAT` (RFC 2389). Empty for a reply with fewer than two `lines`. |
| `(send ?r host-port)` | The host, as a string, and the port, as an integer, of a `227` or `229` reply. A `229` reply has no host, so the host is `""`: use the same server (RFC 2428). Empty for another code or a bad text. |
| `(send ?r restart-markers)` | The two markers of a `110` reply, `MARK yyyy = mmmm`: the user's marker, then the server's, as strings. Empty for another reply. |
| `(send ?r delay)` | The minutes of a `120` reply, `Service ready in nnn minutes`, as an integer. If not, `FALSE`. |
| `(send ?r system)` | The system name of a `215` reply, `NAME system type`, as a symbol. If not, `FALSE`. |

`outcome` uses the first digit of the code and the state diagram of the
verb:

| Diagram | Verbs | `1yz` | `2yz` | `3yz` | `4yz` | `5yz` |
|---|---|---|---|---|---|---|
| simple | `ABOR ALLO DELE CWD CDUP SMNT HELP MODE NOOP PASV QUIT SITE PORT SYST STAT RMD MKD PWD STRU TYPE ACCT RNTO FEAT OPTS EPRT EPSV SIZE MDTM MLST` | error | success | error | failure | failure |
| transfer | `APPE LIST NLST REIN RETR STOR STOU MLSD` | wait | success | error | failure | failure |
| login | `USER PASS` | error | success | continue | failure | failure |
| pending | `RNFR REST` | error | error | continue | failure | failure |
| connection | `CONNECTION` | wait | success | error | failure | failure |

`wait` is a `1yz` reply to a transfer: another reply follows. `continue`
is a `3yz` reply: send the next command, from `next` of the command. A
verb that is not in the diagrams, or a code that is not `1yz` to `5yz`,
is `unknown`.

### `FTP-DATA`

The parent of the data connection instances. A program does not make
one. Each data instance keeps the transfer parameters that the codec read
it with, from `ftp-parse`. `pprint` encodes the instance with them. If
you change the slots, it prints in the new encoding.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `sequence` | The position in the stream. |
| `type` | `ascii`, `ebcdic`, `image` or `local`. The default is `ascii`. |
| `form` | `non-print`, `telnet` or `carriage-control`. The default is `non-print`. |
| `byte-size` | The bits in a logical byte. The default is 8. |
| `structure` | `file`, `record` or `page`. The default is `file`. |
| `mode` | `stream`, `block` or `compressed`. The default is `stream`. |

The `pprint` of a data instance writes octets with `put-char`. Give it
the logical name of a file that is open for writing, or a socket. The
encoder packs the logical bytes into transfer bytes if `byte-size` is not
8. Then it writes the mode:

- Stream mode with file structure writes the bytes. With record
  structure, it writes each 255 as `255 255`. It ends the record with
  `255 1` for EOR, `255 2` for EOF, or `255 3` for both.
- Block mode writes a descriptor and a two byte count before the bytes.
  A record of more than 65535 bytes is more than one block. The
  descriptor has 128 for EOR, 64 for EOF and 32 for suspect.
- Compressed mode writes the escape sequence, 0 and the descriptor,
  before the last string if the record has a descriptor. Two or more
  fillers make a filler string. Three or more of one byte make a
  replicated byte. The filler is a space in ASCII, 64 in EBCDIC, 0 in
  image and local.

### `FTP-RECORD`

One record of the data connection: its logical bytes, as integers. A
file structure makes one record of the whole file. A record structure
makes one record for each EOR. A block or compressed record is all of its
blocks or strings, up to the descriptor that ends it.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `sequence` | The position in the stream. |
| `type`, `form`, `byte-size`, `structure`, `mode` | The transfer parameters. |
| `octets` | The logical bytes, as integers. Each is 0 to 2 to the power of `byte-size`, minus 1. |
| `eor` | `TRUE` if an end-of-record ended it. |
| `eof` | `TRUE` if an end-of-file ended it, or the close of the connection in stream mode with file structure. |
| `suspect` | `TRUE` if a block or compressed string of the record had the suspect code. |

Message-handlers:

| Message-handler | Result |
|---|---|
| `(send ?r pprint ?t)` | Writes the record in its type, structure and mode. |
| `(send ?r logical-bytes)` | The bytes that `pprint` packs: `octets`. |
| `(send ?r text)` | The octets as a string. The handler drops a zero octet: a CLIPS string cannot hold it. |
| `(send ?r lines)` | The text split at CR LF, as strings. A last line without CR LF counts. |
| `(send ?r carriage-controls)` | The vertical spacing that each line asks for, from its first character, for the carriage control form (RFC 959 3.1.1.5.2): `one-line` for a space, `two-lines` for `0`, `new-page` for `1`, `overprint` for `+`. `FALSE` for an empty line or an unknown control. |

### `FTP-PAGE`

One page of a page structure. It is a subclass of `FTP-RECORD`. The
record is the data of the page. The header fields are the page's own
slots. A last page, type 0, ends the file and has `eof` `TRUE`.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `sequence` | The position in the stream. |
| `type`, `form`, `byte-size`, `structure`, `mode` | The transfer parameters. |
| `octets` | The data of the page, as logical bytes. |
| `eor`, `eof`, `suspect` | As in `FTP-RECORD`. `eof` is `TRUE` for a last page. |
| `index` | The page index. The default is 0. |
| `page-type` | 0 last page, 1 simple page, 2 descriptor page, 3 access controlled page. The default is 1. |
| `header` | The optional header fields after the type, as logical bytes. Empty if there are none. |

Message-handlers:

| Message-handler | Result |
|---|---|
| `(send ?p pprint ?t)` | Writes the header, the header fields and the data, in the mode. |
| `(send ?p logical-bytes)` | The bytes that `pprint` packs: the header length, the index, the data length, the page type, the header fields and the data. |
| `(send ?p kind)` | The page type by name: `last`, `simple`, `descriptor` or `access-controlled`. The number of an unknown type. |
| `text`, `lines`, `carriage-controls` | As in `FTP-RECORD`, on the data. |

### `FTP-MARKER`

One restart marker of block or compressed mode (RFC 959 3.5): a block
with descriptor 16, or an escape sequence and a byte string with code 16.
Stream mode has no markers.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `sequence` | The position in the stream. |
| `type`, `form`, `byte-size`, `structure`, `mode` | The transfer parameters. |
| `marker` | The marker as a string of printable characters. |

Message-handlers:

| Message-handler | Result |
|---|---|
| `(send ?m pprint ?t)` | Writes a marker block in block mode. Writes an escape sequence and a byte string of 127 characters maximum in compressed mode. Writes nothing in stream mode. |
