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

For incredibly basic configurations—in particular, if you do not use
grouping at all—an NDBL file is also an executable BASH file which
defines a slew of environment variables.

Structure of an NDBL Document
-----------------------------

All NDBL documents consist of a sequence of multisets of key-value pairs.
All data is represented as text; it is the responsibility of the library
user to parse numeric data, boolean data, &c, during use. NBDL requires
the use of UTF-8 as input and produces UTF-8 chunks when parsed.

A _comment_ is introduced by any whitespace (including newlines)
followed by a pound sign (`#`) and lasts until the end of a line. This
means that a key cannot begin with the `#` character, but that a `#`
character can occur as a constitutent of a key-value pair.

A _key-value pair_ consists of a string of at least length one, followed
by a equals sign (`=`) and subsequently by a string of at least zero.
The value may be quoted, in which case it is allowed to contain any
printable character, including the equals sign, whitespace, and newlines.
An unquoted value is allowed to contain any non-whitespace character
except the equals sign. The value can be zero length. No spaces are
allowed around the equals sign.

A _group_ is a multiset of key-value pairs. A group is introduced by a
non-indented key-value pair; all subsequent key-value pairs on the same
line, as well as any key-value pairs on subsequent indented lines, belong
to the same group.

A _document_ is a sequence of groups.

Examples With Explanation
-------------------------

In the examples below, I will use `{ key: value }` as shorthand to represent a
multimap.

    host=machine1
    host=machine2

This parses to the following structure:

    [{host:machine1},{host:machine2}]

Adding indentation merges the two multisets, as the second line is now
considered a 'cotinuation' of the first group.

    host=machine1
      host=machine2

This becomes

    [{host:machine1,host:machine2}]

A third non-indented line will then start a new group:

    host=machine1
	  host=machine2
	host=machine3

This becomes

    [{host:machine1,host:machine2},{host:machine3}]

Empty values are permitted, and can be used as a tag of sorts.

    database=
	  file=file1.txt
	  file=file2.txt
	  file=file3.txt

This becomes

    [{database:},{file:file1.txt},{file:file2.txt},{file:file3.txt}]

Comments are allowed but must come after a whitespace character, which
means that the following document contains no comments:

    key=value#hello

This becomes

    [{key:value#hello}]

But this document does contain a comment:

    key=value #hello

This becomes

    [{key:value}]

Comments can begin a line, as well.

    # WARNING: do not change
    host=hg-remote
	  portforwarding= # subject to change
	  hostname=hunter-gratzner.example.com
	  port=22
	  user=abu-al-walid
	  nicename="H-G Remote Server"

Haskell API
-----------

Three sets of `encode`/`decode` pairs are provided. One produces a sequential
list of multimap (as implemented by the `multimap` package); one produces
a sequential list of lists of pairs; and one
flattens the list of lists into just a list, for simple situations in which
grouping is irrelevant to the configuration. Each function pair guarantees
that `fromJust . decode . encode == id`, although be aware that
`encode . fromJust . decode == id` is not always true, as multiple valid NDBL
documents may correspond to a single NDBL representation.
