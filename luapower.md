---
tagline: luapower module & package reflection library
---

## `local lp = require'luapower'`

This library leverages the many of the conventions in luapower to extract and
aggregate metadata about packages, modules and documentation and perform
various consistency checks. The entire API is memoized so it can be abused
without worrying about caching the results of the function calls.

Accompanying the library there's an RPC server and a command-line interface.

## Usage

First, you might need to say where the luapower tree is (the default path is '.'):

	lp.config('luapower_dir', '/path/to/luapower')

After that, the API can then be split into the different types of things it does:

  1. getting info about packages and modules in the luapower tree - the bulk
  of the API is for that.
  2. starting an RPC server/connecting to an RPC server and use the API
  remotely, in order to collect data from different platforms.
  3. creating/updating a small database (luapower_db.lua) containing module
  dependencies collected from different platforms.

So the bulk of the API contains stuff like, eg.:

	lp.installed_packages() -> {package = true}      get installed packages
	lp.modules(package) -> {module = path}           get a package's modules

The API is too large for me to describe it all in here, so the functions
are documented in the code instead, so check that out.

## Remote calls

The command to start an RPC server is:

	$ ./luajit luapower_rpc.lua [bind-ip] [port]

To connect to an RPC server, do:

	lp.connect([ip], [port]) -> lp

The result is a full luapower API with the additional functions `close()`,
`restart()`, and `stop()` to control the connection and/or the server.

To update the dependency database, do:

	lp.update_db([package], [platform])

Passing nil as package updates all the packages, same with the platform (so
not passing any args updates the whole db). For this to work, you have to
start an RPC server for each platform, and tell luapower where all those
servers are (see the config table). Needless to say, all servers need to
maintain a copy of the luapower tree which you have to keep synchronized
somehow (NFS, git, rsync, etc.).

## CLI

Finally, for the command-line, type:

	$ ./luapower

The cli also serves as the test unit and demo for the library, so check that
out too.
