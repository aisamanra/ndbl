NDBL
====

NDBL is a basic config file format based roughly on the config format of
Plan 9's [ndb](http://man.cat-v.org/plan_b/6/ndb).

Short Example
-------------

    # A short exemplary snippet of NDBL
    fileset=
	  file=/home/gdritter/english.txt
	  file=/home/gdritter/french.txt
	  file=/home/gdritter/italian.txt
	default=eng
	
	remote=main
	  ip=192.168.1.300
	  user=guest
	  key=public.key

Rationale
---------

NDBL is a very straighforward configuration format, which means it's not
always appropriate for situations in which large amounts of data are
needed. It cannot represent arbitrary hierarchical structures like
JSON, nor does it have the wealth of data types that YAML does. However,
it is a very simple format to both produce and implement (both by
hand and in code) and its simplicity is one of its major virtues. Other
options in this space, but with more structure and extant tooling,
include [YAML](http://en.wikipedia.org/wiki/YAML) and
[TOML](https://github.com/mojombo/toml), both of which are significantly
more complicated than NDBL.

One major advantage of NDBL over other other configuration formats is that
the NDBL grammar is a _regular language_, meaning it can be parsed in
constant space by a finite state machine. While not intended for data
storage, NDBL does possess the two qualities which are important for a
data storage format: it is _self-describing_, meaning that you are
guaranteed to be able to derive the internal representation of the
external (serialized) document, and it is _round-tripping_, meaning that if
we serialize an internal representation to a document and then convert
back to an internal representation, we will obtain an identical internal
representation. Note, for example, that neither XML or YAML necessarily
have this property in practice.

As a pleasant but unintended side effect, an executable bash file that
does nothing but define environment variables is often trivially an NDBL
file.

Structure of an NDBL Document
-----------------------------

All NDBL documents consist of a sequence of multisets of key-value pairs.
All data is represented as text; it is the responsibility of the library
user to parse numeric data, boolean data, &c, during use. NBDL requires
the use of UTF-8 as input and produces UTF-8 chunks when parsed.

A _comment_ is introduced by any whitespace (including newlines)
followed by a pound sign (`#`) and lasts until the end of a line. This
means that a key cannot begin with the `#` character, but that a `#`
character can occur as a constitutent of a key-value pair, including
as a trailing character.

A _key-value pair_ consists of a string of at least length one, followed
by a equals sign (`=`) and subsequently by a string of at least zero.
The key must be a bare string, and can contain any printable non-whitespace
character except the equals sign (`=`) and additionally must not begin
with a pound sign (`#`). It is acceptable for a key to contains a
pound sign if it is not the first character of the key.
The value may be quoted with double quotes (`"`), in which case it is allowed to contain any
printable character, including the equals sign, whitespace, and newlines.
Quoted values understand the escape sequences `\\` for a backslash and
`\"` for a double quote; no other escape sequences are provided.
An unquoted value is allowed to contain any printable non-whitespace
character except the equals sign. The value can be zero length. No spaces are
allowed around the equals sign.

A _group_ is a sequence of key-value pairs. A group is introduced by a
non-indented key-value pair; all subsequent key-value pairs on the same
line, as well as any key-value pairs on subsequent indented lines, belong
to the same group.

A _document_ is a sequence of groups.

Examples With Explanation
-------------------------

In the examples below, I will use `[ key: value ]` as shorthand to represent an
ordered sequence of key-value pairs.

    host=machine1
    host=machine2

This parses to the following structure:

    [[ "host": "machine1" ], [ "host": "machine2" ]]

Adding indentation merges the two multisets, as the second line is now
considered a 'cotinuation' of the first group.

    host=machine1
      host=machine2

This becomes

    [[ "host": "machine1", "host": "machine2" ]]

A third non-indented line will then start a new group:

    host=machine1
	  host=machine2
	host=machine3

This becomes

    [ [ "host": "machine1"
      , "host": "machine2"
	  ]
	, [ "host": "machine3" ]
	]

Empty values are permitted, and are idiomatically used as a 'header' for a group
of configuration options:

    database=
	  file=file1.txt
	  file=file2.txt
	  file=file3.txt

This becomes

    [["database": "", "file": "file1.txt", "file": "file2.txt", "file": "file3.txt"]]

Comments are allowed but must come after a whitespace character, which
means that the following document contains no comments:

    key=value#hello

This becomes

    [["key": "value#hello"]]

But this document does contain a comment:

    key=value #hello

This becomes

    [["key": "value"]]

Comments can begin a line, as well.

    # WARNING: do not change
    host=hg-remote
	  portforwarding= # subject to change
	  hostname=hunter-gratzner.example.com
	  port=22
	  user=abu-al-walid
	  nicename="H-G Remote Server"

This document corresponds to

    [ [ "host": "hg-remote"
      , "portforwarding": ""
	  , "hostname": "hunter-gratzner.example.com"
	  , "port": "22"
	  , "user": "abu-al-walid"
	  , "nicename": "H-G Remote Server"
	  ]
    ]

NDBL does not provide arbitrarily-nested hierarchical strutures,
a list type, a numeric type, a boolean type, or other niceties. This does not
prevent the programmer from interpreting a value as numeric or boolean,
or from using groups in an ad-hoc way to represent hierarchical data—it
merely means that NDBL does not understand those as primitives, nor does it
provide an interface in its API for understanding or manipulating them.
For example, a tree structure could hypothetically be represented
b a series of named nodes

    name=A parent=root
	name=B parent=A
	name=C parent=A
	name=D parent=C

but in the event that such structures arise, it would be better to switch from
NDBL to a proper data storage format.

A Regular Language?
-------------------

I asserted above that NDBL is a regular language. This is true in the sense
that valid NDBL documents can be _recognized_ by a regular language. However,
they cannot be _parsed_ by the kind of regular expressions that you have in
most programming environments: this is because there is no well-founded way
of matching a group underneath a Kleene star. We can construct a regular
expression that matches a group:

~~~~
([^# \t\r\n=][^ \t\r\n=]*)=("[^"|\\\\|\\"]*"|[^ \t\r\n=]*)
~~~~

We could also write a regular expression that maches entire NDBL documents,
using here `{group}` as shorthand for the regex above:

~~~~
({group}((#[^\r\n]*[\r\n]*[ \t]*|[\r\n]*[ \t])*{group})*[\r\n*])*
~~~~

But without
[structural regular expressions](http://9p.io/sources/contrib/steve/other-docs/struct-regex.pdf)
we cannot use this regex to actually pick apart the structure described
by an NDBL document.

Haskell API
-----------

At present, the Haskell API exposes an `encode`/`decode` pair which
(respectively) produce and consume lists of lists of key-value pairs.
Future work involves typeclasses to make consuming and producing datatypes
which correspond to NDBL documents easier.
