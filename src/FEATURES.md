# Introduction and motivation

"Features" is a set of feature names supported by a specific version of
Unison implementation. Over time, each incompatible change -- whether
mandatory or an optional add-on -- is assigned a unique feature name.

Features allow a client to connect to and properly work with a server of
different version, older or newer. When setting up the connection, both
server and client negotiate a commonly supported set of features.

Using features instead of a version makes the implementation agnostic of any
versioning schemes, forks and third party implementations. It also allows
for more flexible code changes over time, without the code being polluted by
adding more and more conditionals for various version combinations, such as
"if version < X then", "if version >= Y and version < Z then", and so on.

# Negotiation

Feature negotiation takes place immediately after the RPC connection has
been fully set up. See `negotiate.ml`.

1. Client sends its full feature set to the server.
2. Server validates the intersection of its and client's feature sets.
   - If error then server sends NOK to client. The client closes connection.
3. If OK then server sends intersection of feature sets to the client.
4. Client validates the intersection.
   - If error then client closes connection.
5. If OK then the negotiation is complete and both server and client will
   use only features fully supported by both.

## Feature registration

A feature is added to the set by registering it. This can be done by any
part of the code that "owns" a feature, similar to how user preferences are
registered. See `features.mli` and `features.ml`.

Registering a feature requires a unique feature name and an optional
validation function.

## Feature validation

Each feature can provide a separate validation function. When validating
the intersection of client's and server's feature sets, validation
functions for each included feature are run in arbitrary sequence.

A validation function will be able to see the entire intersection and can
freely decide whether the intersection is ok or not. Examples of possible
validation scenarios:

- A mandatory feature is not in the intersection
  - This typically means that counterparty is too old, but could also mean
    that the counterparty is too new and the feature has been removed.
- User preference enabled for a feature not in intersection
  (the preferences have not been sent to the server yet, so this
  validation is not carried out by the server)
- A feature depends on another feature not in intersection

Some features in the intersection can conflict with each other. This can
happen for example when two different implementations of a function are
both supported but must not be used simultaneously. All such conflicts
are benign in nature and will not cause feature intersection validation
to fail. (Since the intersection is a subset of the entire feature set
then failing a conflict would mean that the set of features is conflicting
to begin with.)

# Development

Every incompatible code change must result in a change to the set of
features:

- New code that is mandatory to use (effectively breaks compatibility
  with older versions despite feature negotiation) ->
  - Register a new feature with a validation function that rejects any
    feature intersection that does not include this feature
- New code that is optional to use ->
  - Register a new feature
- New code that replaces existing code ->
  - Register a new feature and remove one or more features
- Remove existing code ->
  - Remove one or more features
- Code is not removed but it can be deprecated ->
  - Add or change a validation function to output a deprecation warning

## User preferences

User preferences are sent from client to server after establishing a
connection. The server must know all preferences received from the client,
otherwise the connection fails.

When new preferences are created with a new feature, it is possible (and in
most cases required) to add a guard function that determines if the
preference is sent to the server or not. Typically, this guard function
will take the form `fun () -> Features.enabled somefeature`, meaning that
the preference is sent to server if and only if 'somefeature' is known by
the server.

## Code evolution, conflicting features

With features, new code does not have to replace existing code even if
they seemingly conflict. Both an existing feature and a new feature can
co-exist. The code must be guarded by checking which features are enabled
at runtime for each remote connection.

For example:

- Existing code implements feature hash-1.
- New code implements a new hashing algorithm and adds feature hash-2.
- Even though two different hashing algorithms must not be used at the
  same time, both implementations can co-exist as in the following
  pseudocode example.

```
function hash
  if (feature hash-2 enabled) then
    new algorithm
  else if (feature hash-1 enabled) then
    previous algorithm
  end
```

- If both server and client support hash-2 then the new implementation
  will always be used, even if both server and client also support hash-1
  at the same time.
- If either server or client does not support hash-2 then the feature
  intersection will only contain hash-1 and the previous implementation
  will be used.

Now let's imagine that in addition to hashing algorithm changing with
the new feature, also the result type changes. This is trickier to implement
but clearly not impossible.

There are multiple ways of handling parallel implementation of conflicting
types. These are not the topic of this document, but a few possibilities
are provided for inspiration:

- Abstract types and type variables
- Variant types (aka sum types)
- Extensible variant types
- First class modules
- GADTs
- Classes/objects

### Archive file

Most changes will ultimately result in type changes. This will directly
impact data encoded in wire format and stored in archive file format.

Data on the wire is transient. As both client and server have agreed on
a common feature set, they know how to marshal and unmarshal data on the
wire without any issues.

Data in the archive file is persistent and could have been written while
a different set of features was agreed upon. There are a couple of ways
to read and write archive files in this scenario:

- Not even attempt to read an incompatible archive file. The exact used
  feature set is written into the archive file. As long as both client and
  server keep negotiating the same feature set, they can read existing
  archive files. When the negotiated feature set changes (due to upgrades),
  the previous archive files can be ignored (requires a complete rescan).
  This may be acceptable, as such upgrades are assumed to be quite rare.

- A subset of the used feature set is written into the archive file. Only
  features that change the data structures written in the archive file are
  stored in the file. The reading can work in two ways. Either as a slightly
  more forgiving variant of the point above, or actually reading and
  unmarshaling the archive according to the features used to write it --
  even if not all the same features are included in the currently negotiated
  feature set. The latter is the currently chosen approach. It does require
  types and code be tailored for this, the same as with the next point below,
  but to a lesser degree.

- The archive file on-disk format includes information about the types and
  structure of the written data (you can think like a DB with a relatively
  dynamic but still typed schema). The data can be read back selectively,
  and even converted as necessary (for example, can read a stored int32 into
  in-memory int64).
  The selective reading can mean two things. First, the archive was written
  with a feature that is no longer enabled. The data that was only relevant
  to that feature is just skipped. Second, the archive was written without
  a feature that is now enabled. For the newly-enabled feature there is no
  data in the archive but this does not break reading the file, as long as
  the new feature can deal with default or "empty" values for its data
  structures.

