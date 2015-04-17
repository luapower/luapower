---
tagline: luapower package reflection library
---

## `local lp = require'luapower'`

This module leverages the many conventions in luapower to extract and
aggregate metadata about packages, modules and documentation and perform
various consistency checks. It gives accurate information about dependencies
between modules and packages because it actually loads the Lua modules and
tracks all `require` and `ffi.load` calls, and then it integrates that
information with the package information that it gets from git and multigit.
The entire API is memoized so it can be abused without worrying about
caching the results of the function calls.

Accompanying the library there's a command-line interface and an RPC server
which can be used to track module dependencies across multiple platforms,
run automated tests, etc.

## Module usage

The module assumes that the luapower tree is at the current directory.
If that's not the case, you have say where it is:

	lp.config('luapower_dir', '/path/to/luapower')

The API can be categorized based on the different types of things it does:

  1. getting info about packages and modules in the luapower tree - this is
  the bulk of the API.
  2. starting an RPC server/connecting to an RPC server and using the API
  remotely, in order to collect data from different platforms.
  3. creating/updating a small database (luapower_db.lua) containing module
  dependencies collected from different platforms.

So the bulk of the API contains stuff like, eg.:

	lp.installed_packages() -> {package = true}      get installed packages
	lp.modules(package) -> {module = path}           get a package's modules

The API is too large for me to describe it all in here, so the functions
are documented in the code instead, so check that out.

## Remote usage

The command to start an RPC server is:

	$ ./luajit luapower_rpc.lua [IP] [PORT]

To connect to an RPC server, do:

	lp.connect([ip], [port]) -> lp

The result is a full luapower API with the additional functions `close()`,
`restart()`, and `stop()` to control the connection and/or the server.

Each connection gets its own separate Lua state to do stuff in, so the
luapower cache is lost when the connection is closed.

To update the dependency database, do:

	lp.update_db([package], [platform])

Passing nil as package updates all the packages, same with the platform
(so not passing any args updates the whole db). For this to work, you have
to start an RPC server for each platform, and tell luapower where all those
servers are (see the config table). Needless to say, all servers need to
maintain a copy of the luapower tree which you have to keep synchronized
somehow (NFS, samba, git, rsync, etc.).

## Command line

Finally, for the command-line, type:

	$ ./luapower

The cli also serves as the test unit and demo for the library,
so check that out too.
