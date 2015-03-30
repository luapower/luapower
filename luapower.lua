
--luapower package reflection library.
--Written by Cosmin Apreutesei. Public Domain.

local luapower = setmetatable({}, {__index = _G})
setfenv(1, luapower)

local lfs = require'lfs'
local glue = require'glue'
local ffi = require'ffi'
local pp = require'pp'
local libgit2 = ffi.os == 'OSX' and require'libgit2'
local luastate = require'luastate'

--config

local cfg = {
	luapower_dir = '.',
	git_base_dir = '_git',
	oses = {mingw = true, linux = true, osx = true},
	platforms = {
		mingw32 = true, mingw64 = true,
		linux32 = true, linux64 = true,
		osx32 = true, osx64 = true,
	},
	servers = ffi.os ~= 'Linux' and {
		linux32 = {'86.105.182.2', '1994'},
		linux64 = {'86.105.182.2', '1999'},
		mingw32 = {'86.105.182.2', '1995'},
		mingw64 = {'86.105.182.2', '1996'},
		osx32   = {'86.105.182.2', '1998'},
		osx64   = {'86.105.182.2', '1997'},
	} or {
		linux32 = {'172.16.134.130'},
		linux64 = {'127.0.0.1'},
		mingw32 = {'172.16.134.131'},
		mingw64 = {'172.16.134.133'},
		osx32   = {'172.16.134.128', '19993'},
		osx64   = {'172.16.134.128'},
	},
}

function config(var, val)
	if val ~= nil then
		cfg[var] = val
	else
		return cfg[var]
	end
end

local function plusfile(file)
	return file and '/'..file or ''
end

function powerpath(file)
	return config'luapower_dir'..plusfile(file)
end

function gitpath(file)
	return powerpath(config'git_base_dir'..plusfile(file))
end

--memoize with total and partial cache invalidation

--memoize that cannot have its cache cleared.
local memoize_permanent = glue.memoize

--memoize of 1 and 2 arg functions, where the first arg is always a package.
--cache can be cleared for individual packages.
local pkg_caches = {} --{func = {pkg = val}}
local function memoize_package(func)
	return glue.memoize(func, glue.attr(pkg_caches, func))
end

--generic memoize that can only have its entire cache cleared.
local rememoizers = {}
local function memoize(func)
	local memfunc
	local function rememoize()
		memfunc = glue.memoize(func)
	end
	rememoize()
	rememoizers[func] = rememoize
	return function(...)
		return memfunc(...)
	end
end

function clear_cache(pkg)
	if pkg then
		for _, cache in pairs(pkg_caches) do
			cache[pkg] = nil
		end
	else
		for _, cache in pairs(pkg_caches) do
			for pkg in pairs(cache) do
				cache[pkg] = nil
			end
		end
	end
	for _, rememoize in pairs(rememoizers) do
		rememoize()
	end
	collectgarbage() --unload modules from the tracking Lua state
end

--other helpers

local function filter(t, f)
	local dt = {}
	for k,v in pairs(t) do
		if f(k,v) then
			dt[k] = v
		end
	end
	return dt
end

local empty = {}


--data acquisition: readers and parsers
--============================================================================


--detect current platform

local platos = {Windows = 'mingw', Linux = 'linux', OSX = 'osx'}
current_platform = memoize_permanent(function()
	return platos[ffi.os]..(ffi.abi'32bit' and '32' or '64')
end)

function check_platform(platform)
	if not platform then
		return current_platform()
	end
	glue.assert(config('platforms')[platform], 'unknown platform "%s"', platform)
	return platform
end


--find dependencies of a module by tracing the `require` and `ffi.load` calls.
------------------------------------------------------------------------------

--modules that we won't track because require'ing them
--is not necessary in any Lua version.
builtin_modules = {
	string = true, table = true, coroutine = true, package = true, io = true,
	math = true, os = true, _G = true, debug = true,
}

--install `require` and `ffi.load` trackers -- to be run in a new Lua state.
local function install_trackers(builtin_modules, filter)

	--find Lua dependencies of a module by tracing its `require` calls.

	local parents = {} --{module1, ...}
	local dt = {} --{module = dep_table}
	local lua_require = require

	function require(m)

		--create the module's tracking table
		dt[m] = dt[m] or {}

		--require the module directly if it doesn't need tracking.
		if builtin_modules[m] then
			return lua_require(m)
		end

		--register the module as a dependency for the parent at the top of the stack
		local parent = parents[#parents]
		if parent then
			dt[parent].mdeps = dt[parent].mdeps or {}
			dt[parent].mdeps[m] = true
		end

		--push the module into the parents stack
		table.insert(parents, m)

		--check the error cache before loading the module
		local ok, ret
		local err = dt[m].loaderr
		if err then
			ok, ret = nil, err
		else
			ok, ret = pcall(lua_require, m)

			if not ok then
			   --cache the error for future calls.
				local err = ret:gsub(':?%s*[\n\r].*', '')
				--remove source info for platform and arch load errors
				if err:find'platform not ' and not err:find'arch not ' then
					err = err:gsub('[^:]*:%d+: ', '')
				end
				dt[m] = {loaderr = err}
			end
		end

		--pop the module from the parents stack
		table.remove(parents)

		if not ok then
			error(ret, 2)
		end

		--copy the module's autoload table if it has one
		--TODO: dive into the keys of module and check autoload on the keys too!
		--eg. bitmap: 'colortypes.rgbaf' -> 'bitmap_rgbaf'
		local mt = getmetatable(ret)
		local auto = mt and rawget(mt, '__autoload')
		if auto then
			dt[m].autoloads = filter(auto, function(key, mod)
				return type(key) == 'string' and type(mod) == 'string'
			end)
		end

		return ret
	end

	--find C dependencies of a module by tracing the `ffi.load` calls.

	local ffi = lua_require'ffi'
	local ffi_load = ffi.load

	function ffi.load(clib, ...)
		local ok, ret = xpcall(ffi_load, debug.traceback, clib, ...)
		local m = parents[#parents]
		local t = dt[m]
		t.ffi_deps = t.ffi_deps or {}
		t.ffi_deps[clib] = ok
		if not ok then
			error(ret, 2)
		else
			return ret
		end
	end

	function track_module(m, loader_m)
		if loader_m then
			local ok, ret = pcall(require, loader_m)
			if not ok then --put the error on account of mod
				dt[m] = {loaderr = err} --clear deps
				return t
			end
		end
		pcall(require, m)
		return dt[m]
	end

end

local tracking_state = memoize(function()
	local state = luastate.open()
	state:openlibs()
	state:push(install_trackers)
	state:call(builtin_modules, filter)
	return state
end)

local function track_module(m, loader_m)
	assert(m, 'module required')
	local state = tracking_state()
	state:getglobal'track_module'
	return state:call(m, loader_m)
end

--dependency tracking based on parsing
------------------------------------------------------------------------------

--luajit built-in modules that don't have source code to inspect.
luajit_builtin_modules = {
	ffi = true, bit = true, jit = true,
	['jit.util'] = true, ['jit.profile'] = true,
}

module_requires_parsed = memoize(function(m) --direct dependencies
	local t = {}
	if builtin_modules[m] or luajit_builtin_modules[m] then
		return t
	end
	local path =
		package.searchpath(m, package.path)
		or package.searchpath(m, package.path:gsub('%.lua', '.dasl'))
	if not path then
		return t
	end
	local s = assert(glue.readfile(path))
	--delete long comments
	s = s:gsub('%-%-%[(=*)%[.*%]%1%]', '')
	--delete long strings
	s = s:gsub('%-%-%[%[.*%]%]', '')
	--delete short comments
	s = s:gsub('%-%-[^\n\r]*', '')
	--delete the demo section
	s = s:gsub('[\r\n]if not %.%.%. then.*', '')
	--require'xxx'
	for m in s:gmatch'require%s*(%b\'\')' do
		t[m:sub(2,-2)] = true
	end
	--require"xxx"
	for m in s:gmatch'require%s*(%b"")' do
		t[m:sub(2,-2)] = true
	end
	--require("xxx") or require('xxx')
	for m in s:gmatch'require%s*(%b())' do
		m = glue.trim(m:sub(2,-2))
		if m:find'^%b\'\'$' or m:find'^%b""$' then
			m = m:sub(2,-2)
			if m:find'^[a-z0-9%.]+$' then
				t[m] = true
			end
		end
	end
	return t
end)


--filesystem reader
------------------------------------------------------------------------------

--recursive lfs.dir() -> iter() -> filename, path, mode
local function dir(p0, recurse)
	assert(p0)
	local t = {}
	local function rec(p)
		local dp = p0 .. (p and '/' .. p or '')
		for f in lfs.dir(dp) do
			if f ~= '.' and f ~= '..' then
				local mode = lfs.attributes(dp .. '/' .. f, 'mode')
				table.insert(t, {f, p, mode})
				if recurse and mode == 'directory' then
					rec((p and p .. '/' .. f or f))
				end
			end
		end
	end
	rec()
	local i = 0
	return function()
		i = i + 1
		if not t[i] then return end
		return unpack(t[i], 1, 3)
	end
end

--path/dir/file -> path/dir, file
local function split_path(path)
	local filename = path:match'([^/]*)$'
	local n = #path - #filename - 1
	if n > 1 then n = n - 1 end --remove trailing '/' if the path is not '/'
	return path:sub(1, n), filename
end


--git command output readers
------------------------------------------------------------------------------

--read a cmd output to a line iterator
local function pipe_lines(cmd)
	if ffi.os == 'Windows' then
		cmd = cmd .. ' 2> nul'
	else
		cmd = cmd .. ' 2> /dev/null'
	end
	local pwd = lfs.currentdir()
	lfs.chdir(powerpath())
	local t = {}
	glue.fcall(function(finally, onerror)
		local f = assert(io.popen(cmd, 'r'))
		finally(function()
			f:close()
			lfs.chdir(pwd)
		end)
		f:setvbuf'full'
		for line in f:lines() do
			t[#t+1] = line
		end
	end)
	local i = 0
	return function()
		i = i + 1
		return t[i]
	end
end

--read a cmd output to a string
local function read_pipe(cmd)
	local t = {}
	for line in pipe_lines(cmd) do
		t[#t+1] = line
	end
	return table.concat(t, '\n')
end

local function git_dir(package)
	if package == 'luapower-git' then
		return powerpath'.git'
	end
	return gitpath(package..'/.git')
end

--git command string for a package repo
local function gitp(package, args)
	local git = ffi.os == 'Windows' and 'git.exe' or 'git'
	return git..' --git-dir="'..git_dir(package)..'" '..args
end

function git(package, cmd)
	return read_pipe(gitp(package, cmd))
end

function gitlines(package, cmd)
	return pipe_lines(gitp(package, cmd))
end

function repo(package)
	return libgit2.open(git_dir(package))
end


--module finders
------------------------------------------------------------------------------

--path/*.lua -> Lua module name
local function lua_module_name(path)
	if path:find'^bin/[^/]+/lua/' then --platform-dependent module
		path = path:gsub('^bin/[^/]+/lua/', '')
	end
	return path:gsub('/', '.'):match('(.-)%.lua$')
end

--path/*.dasl -> dasl module name
local function dasl_module_name(path)
	return path:gsub('/', '.'):match('(.-)%.dasl$')
end

--path/*.dll|.so -> C module name
local function c_module_name(path)
	local ext = package.cpath:match'%?%.([^;]+)' --dll, so
	local name = path:match('bin/[^/]+/clib/(.-)%.'..ext..'$')
	return name and name:gsub('/', '.')
end

local function module_name(path)
	return 
		lua_module_name(path) or 
		dasl_module_name(path) or 
		c_module_name(path)
end

--'module_submodule' -> 'module'; 'module.submodule' -> 'module'
local function parent_module_name(mod)
	local parent = mod:match'(.-)[_%.][^_%.]+$'
	if not parent or parent == '' then return end
	return parent
end


--tree builder and tree walker patterns
------------------------------------------------------------------------------

--tree builder based on a function that produces names and a function that
--resolves the parent name of a name.
local function build_tree(get_names, get_parent)
	local parents = {}
	for name in get_names() do
		parents[name] = get_parent(name) or true
	end
	local root = {name = true}
	local function add_children(pnode)
		for name, parent in pairs(parents) do
			if parent == pnode.name then
				local node = {name = name}
				pnode.children = pnode.children or {}
				table.insert(pnode.children, node)
				add_children(node)
			end
		end
	end
	add_children(root)
	return root
end

--tree walker for nested arrays. depth-first traversal.
function walk_tree(t, f)
	local function walk_children(pnode, level)
		if type(pnode) ~= 'table' then return end
		if not pnode.children then return end
		for i,node in ipairs(pnode.children) do
			f(node, level, pnode, i)
			walk_children(node, level + 1)
		end
	end
	walk_children(t, 0)
end


--WHAT file parser
------------------------------------------------------------------------------

--WHAT file -> {realname=, version=, url=, license=, dependencies={d1,...}}
local function parse_what_file(what_file)
	local t = {}
	local more, close = assert(more(what_file))

	--parse the first line which has the format:
	--		'<realname> <version> from <url> (<license>)'
	local s = assert(more(), 'invalid WHAT file '.. what_file)
	t.realname, t.version, t.url, t.license =
		s:match('^%s*(.-)%s+(.-)%s+from%s+(.-)%s+%((.*)%)')
	if not t.realname then
		error('invalid WHAT file '.. what_file)
	end
	t.license = t.license and
		t.license:match('^(.-)%s+'..glue.escape('license', '*i')..'$')
		or t.license
	t.license =
		t.license:match('^'..glue.escape('public domain', '*i')..'$')
		and 'PD' or t.license

	--parse the second line which has the format:
	--		'requires: <pkg1>, <pkg2> (<platform1> ...), ...'
	t.dependencies = {} -- {platform = {dep = true}}
	local s = more()
	s = s and s:match'^[^:]*:(.*)'
	if s then
		for s in glue.gsplit(s, ',') do
			s = glue.trim(s)
			if s ~= '' then
				local s1, ps = s:match'^([^%(]+)%s*%(%s*([^%)]+)%s*%)' --'pkg (platform1 ...)'
				if ps then
					s = s1
					for platform in glue.gsplit(ps, '%s+') do
						glue.attr(t.dependencies, platform)[s] = true
					end
				else
					for platform in pairs(config'platforms') do
						glue.attr(t.dependencies, platform)[s] = true
					end
				end
			end
		end
	end

	close()
	return t
end


--markdown yaml header parser
------------------------------------------------------------------------------

--"key <separator> value" -> key, value
local function split_kv(s, sep)
	sep = glue.escape(sep)
	local k,v = s:match('^([^'..sep..']*)'..sep..'(.*)$')
	k = k and glue.trim(k)
	if not k then return end
	v = glue.trim(v)
	if v == '' then v = true end --values default to true in pandoc
	return k,v
end

--parse the yaml header of a pandoc .md file, enclosed by '---\n'
local function parse_md_file(md_file)
	local docname = md_file:match'([^/\\]+)%.md$'
	local t = {}
	local more, close = more(md_file)
	if not more or more() ~= '---' then
		t.title = docname
		close()
		return t
	end
	for s in more do
		if s == '---' then break end
		local k,v = split_kv(s, ':')
		if not k then
			error('invalid tag '..s)
		elseif t[k] then
			error('duplicate tag '..k)
		else
			t[k] = v
		end
	end
	t.title = t.title or docname --set default title
	close()
	return t
end

--cat.md parser
------------------------------------------------------------------------------

--parse the table of contents file into a list of categories and docs.
cats = memoize_package(function(package)
	local more, close = assert(more(gitpath'cat.md'))
	local cats = {}
	local lastcat
	local misc
	local pkgs = installed_packages()
	local uncat = glue.update({}, pkgs)
	for s in more do
		local pkg = s:match'^%s*%*%s*%[([^%]]+)%]%s*$' -- " * [name]"
		if pkg then
			if pkgs[pkg] then
				table.insert(lastcat.packages, pkg)
				uncat[pkg] = nil
			end
		else
			local cat = s:match'^%s*%*%s*(.-)%s*$' -- " * name"
			if cat then
				lastcat = {name = cat, packages = {}}
				table.insert(cats, lastcat)
				if cat == 'Misc' then
					misc = lastcat
				end
			end
		end
	end
	if not misc then
		table.insert(cats, {name = 'Misc', packages = glue.keys(uncat, true)})
	end
	close()
	return cats
end)


--data acquisition: logic and collection
--============================================================================


--packages and their files
------------------------------------------------------------------------------

--_git/<name>.origin -> {name = true}
known_packages = memoize(function()
	local t = {}
	for f in dir(powerpath(config'git_base_dir')) do
		local s = f:match'^(.-)%.origin$'
		if s then t[s] = true end
	end
	return t
end)

--_git/<name>/.git -> {name = true}
installed_packages = memoize(function()
	local t = {}
	for f, _, mode in dir(powerpath(config'git_base_dir')) do
		if mode == 'directory'
			and lfs.attributes(git_dir(f), 'mode') == 'directory'
		then
			t[f] = true
		end
	end
	return t
end)

--(known - installed) -> not installed
not_installed_packages = memoize(function()
	local installed = installed_packages()
	return filter(known_packages(),
		function(pkg) return not installed[pkg] end)
end)

--wrapper for any function(package, ...) that returns a table with keys that
--are unique accross all packages. it makes the package argument optional
--so that if not given, function(package) is called repeatedly for each
--installed package and the results are accumulated into a single table.
local function opt_package(func)
	return function(package, ...)
		if package then
			return func(package, ...)
		end
		local t = {}
		for package in glue.sortedpairs(installed_packages()) do
			glue.update(t, func(package, ...))
		end
		return t
	end
end

local pkgt = {}

local function memoize_opt_package(func)
	return memoize(opt_package(memoize_package(func)))
end

--git ls-files -> {path = package}
tracked_files = memoize_opt_package(libgit2 and function(package)
	local repo = repo(package)
	local ref = repo:ref_dwim'master'
	local id = repo:ref_name_to_id(ref:name())
	local commit = repo:commit(id)
	local tree = commit:tree()
	local t = {}
	for path in repo:files(tree) do
		t[path] = package
	end
	tree:free()
	commit:free()
	ref:free()
	repo:free()
	return t
end or function(package)
	local t = {}
	for path in gitlines(package, 'ls-files') do
		t[path] = package
	end
	return t
end)

function more(filename)
	local f, err = io.open(filename, r)
	if not f then return nil, err end
	local function more()
		local s = f:read'*l'
		if not s then f:close(); f = nil end
		return s
	end
	local function close()
		if f then f:close() end
	end
	return more, close
end


--tracked files breakdown: modules, scripts, docs
------------------------------------------------------------------------------

--check if a path is valid for containing modules.
local function is_module_path(p, platform)
	platform = platform and check_platform(platform) or '[^/]+'
	return not p or not (
		(p:find'^bin/'
			and not p:find('^bin/'..platform..'/clib/')
			and not p:find('^bin/'..platform..'/lua/'))
		or p:find'^csrc/'
		or p:find'^media/'
	)
end

--check if a path is valid for containing docs
local function is_doc_path(p)
	return not p or not (
		p:find'^bin/'
		or p:find'^csrc/'
		or p:find'^media/'
	)
end

--check if a name is a module as opposed to a script or app
local function is_module(mod)
	return not (
		mod:find'_test$'
		or mod:find'_demo$'
		or mod:find'_demo_.*$' --"demo_<arch>"
		or mod:find'_benchmark$'
		or mod:find'_app$'
	)
end

--tracked <doc>.md -> {doc = path}
local docs_ = opt_package(memoize_package(function(package)
	local t = {}
	for path in pairs(tracked_files(package)) do
		if is_doc_path(path) then
			local dir, file = split_path(path)
			local name = file:match'^(.-)%.md$'
			if name then
				if t[name] then
					error('duplicate doc '..name..' as '..t[name]..' and '..path)
				end
				t[name] = path
			end
		end
	end
	return t
end))
docs = memoize(function(package)
	local t = docs_(package)
	if not package then
		--luapower-git contains docs too, but they're not owned by any package
		glue.update(t, docs_'luapower-git')
	end
	return t
end)

--FIXME: current platform is assumed for Lua/C module paths.
local function modules_(package, should_be_module)
	local t = {}
	for path in pairs(tracked_files(package)) do
		if is_module_path(path, current_platform()) then
			local mod = module_name(path)
			if mod and is_module(mod) == should_be_module then
				t[mod] = path
			end
		end
	end
	if should_be_module and package == 'luajit' then
		glue.update(t, builtin_modules, luajit_builtin_modules)
	end
	return t
end

--tracked <module>.lua -> {module = path}
modules = memoize_opt_package(function(package) 
	return modules_(package, true) 
end)

--tracked <script>.lua -> {script = path}
scripts = memoize_opt_package(function(package)
	return modules_(package, false) 
end)

--tracked file -> {path = type}
file_types = memoize_opt_package(function(package)
	local t = {}
	for path in pairs(tracked_files(package)) do
		if is_module_path(path) then
			local mod = module_name(path)
			if mod then
				t[path] = is_module(mod) and 'module' or 'script'
			elseif is_doc_path(path) then
				t[path] = 'doc'
			else
				t[path] = 'unknown'
			end
		end
	end
	return t
end)


--module logical (name-wise) tree
------------------------------------------------------------------------------

--first ancestor module (parent, grandad etc) that actually exists in the same
--package (or in all packages).
local function module_parent_(package, mod)
	local parent = parent_module_name(mod)
	if not parent then return end
	return modules(package)[parent] and parent 
		or module_parent_(package, parent)
end
local module_parent = memoize_package(module_parent_)

--build a module tree for a package (or for all packages)
module_tree = memoize_package(function(package)
	local function get_names() return pairs(modules(package)) end
	local function get_parent(mod) return module_parent(package, mod) end
	return build_tree(get_names, get_parent)
end)


--doc tags
------------------------------------------------------------------------------

docfile_tags = memoize(parse_md_file)

--tracked <doc>.md -> {title='', project='', other yaml tags...}
doc_tags = memoize_package(function(package, doc)
	local path = docs(package)[doc]
	return path and docfile_tags(powerpath(path))
end)


--reverse lookups
------------------------------------------------------------------------------

--reverse lookup of a package from a module
module_package = memoize(function(mod)
	assert(mod, 'module required')
	--shortcut: builtin module
	if builtin_modules[mod] then return end
	if luajit_builtin_modules[mod] then return 'luajit' end
	--shortcut: find the package that matches the module name or one of its
	--prefixes (which is a possible parent module).
	local mod1 = mod
	while mod1 do
		if installed_packages()[mod1] then
			if modules(mod1)[mod1] then --the module is indeed in the package
				return mod1
			end
		end
		mod1 = parent_module_name(mod1)
	end
	--the slow way: look in all packages for the module
	--print('going slow for '..mod..'...')
	local path = modules()[mod]
	return path and tracked_files()[path]
end)

--memoize that can be used with f(mod, package) where package is optional.
local function memoize_mod_package(func)
	local memfunc = memoize_package(function(package, mod)
		return func(mod, package)
	end)
	return function(mod, package)
		package = package or module_package(mod)
		return memfunc(package, mod)
	end
end

--reverse lookup of a package from a doc
doc_package = function(doc)
	--shortcut: package doc
	if installed_packages()[doc] and docs(doc)[doc] then
		return doc
	elseif docs('luapower-git')[doc] then
		return 'luapower-git'
	end
	--the slow way: look in all packages for the doc
	--print('going slow for '..doc..'...')
	local path = docs()[doc]
	return path and tracked_files()[path]
end

--reverse lookup of a package from a ffi module.
--ffi modules are binaries loaded with ffi.load('<mod>').
local libfmt = {
	mingw32 = '%s.dll', mingw64 = '%s.dll',
	linux32 = 'lib%s.so', linux64 = 'lib%s.so',
	osx32 = 'lib%s.dylib', osx64 = 'lib%s.dylib',
}

local function ffi_module_in_package(mod, package, platform)
	local path = 'bin/'..platform..'/'..string.format(libfmt, mod)
	return tracked_files(package)[path] and true or false
end

ffi_module_package = memoize(function(mod, package, platform, ...)
	platform = check_platform(platform)
	--shortcut: try current package.
	if package and ffi_module_in_package(mod, package, platform) then
		return package
	end
	--shortcut: find the package that matches the module name.
	if installed_packages()[mod] then
		if ffi_module_in_package(mod, mod, platform) then
			return mod
		end
	end
	--slow way: look in all packages
	for package in pairs(installed_packages()) do
		if ffi_module_in_package(mod, package, platform) then
			return package
		end
	end
end)


--package csrc info
------------------------------------------------------------------------------

csrc_dir = memoize_package(function(package) --there should be only one csrc dir per package
	--shortcut: csrc dir matches package name
	if lfs.attributes(powerpath('csrc/'..package), 'mode') == 'directory' then
		return 'csrc/'..package
	end
	for path in pairs(tracked_files(package)) do
		local dir = path:match'^(csrc/[^/]+)/'
		if dir then return dir end
	end
end)

--csrc/*/WHAT -> {tag=val,...}
c_tags = memoize_package(function(package)
	if not csrc_dir(package) then return end
	local what_file = powerpath(csrc_dir(package) .. '/WHAT')
	return glue.fileexists(what_file) and parse_what_file(what_file)
end)

local has_luac_modules = memoize_package(function(package)
	for mod, path in pairs(modules(package)) do
		if type(path) == 'string' and c_module_name(path) then
			return true
		end
	end
end)

--package dependencies as declared in the WHAT file
bin_deps = memoize_package(function(package, platform)
	platform = check_platform(platform)
	local t = c_tags(package) and c_tags(package).dependencies
	t = t and t[platform] or {}
	--packages containing Lua/C modules have an implicit dependency 
	--on luajit on Windows.
	if platform:find'^mingw' and has_luac_modules(package) then
		t.luajit = true
	end
	return t
end)

--platforms are inferred from the name of the build script:
--csrc/*/build-<platform>.sh -> {platform = true,...}
build_platforms = memoize_opt_package(function(package)
	local t = {}
	if csrc_dir(package) then
		for path in pairs(tracked_files(package)) do
			local platform = path:match('^'..
				glue.escape(csrc_dir(package)..'/build-')..'(.-)%.sh$')
			if platform and config('platforms')[platform] then
				t[platform] = true
			end
		end
	end
	return t
end)

--platforms can also be inferred from the presence of files in bin/<platform>.
bin_platforms = memoize_opt_package(function(package)
	local t = {}
	for path in pairs(tracked_files(package)) do
		local platform = path:match('^bin/([^/]+)/.')
		if platform then
			t[platform] = true
		end
	end
	return t
end)

--platforms can be specified in the 'platforms' tag of the package doc file:
--<package>.md:platforms -> {platform = true,...}
declared_platforms = memoize_opt_package(function(package)
	local t = {}
	local tags = doc_tags(package, package)
	if tags and tags.platforms then
		for platform in glue.gsplit(tags.platforms, ',') do
			platform = glue.trim(platform)
			if platform ~= '' then
				t[platform] = true
			end
		end
	end
	return t
end)

platforms = memoize_opt_package(function(package)
	return glue.update({},
		build_platforms(package),
		bin_platforms(package),
		declared_platforms(package))
end)


--package git info
------------------------------------------------------------------------------

--current git version
git_version = memoize_package(libgit2 and function(package)
	local repo = repo(package)
	local ref = repo:ref_dwim'master'
	local id = repo:ref_name_to_id(ref:name())
	local obj = repo:object(id, 'commit')
	local ok, ver = pcall(obj.describe_commit, obj, {
			describe_strategy = libgit2.C.GIT_DESCRIBE_TAGS, --tags
			always_use_long_format = true, --long
			show_commit_oid_as_fallback = true, --always
		})
	obj:free()
	ref:free()
	repo:free()
	return ok and ver or 'n/a'
end or function(package)
	return git(package, 'describe --tags --long --always')
end)

--list of tags
git_tags = memoize_package(libgit2 and function(package)
	local repo = repo(package)
	local tags = repo:tags()
	repo:free()
	return tags
end or function(package)
	local t = {}
	for tag in gitlines(package, 'tag') do
		t[#t+1] = tag
	end
	return t
end)

--current tag
git_tag = memoize_package(libgit2 and function(package)
	local tags = git_tags(package)
	return tags[#tags] or ''
end or function(package)
	return git(package, 'describe --tags --abbrev=0')
end)

git_origin_url = memoize_package(libgit2 and function(package)
	local repo = repo(package)
	local cfg = repo:config()
	local url = cfg:get'remote.origin.url'
	cfg:free()
	repo:free()
	return url
end or function(package)
	return git(package, 'config --get remote.origin.url')
end)

git_mtime = memoize_package(libgit2 and function(package, file)
	--TODO: implement the file arg
	--see https://github.com/libgit2/libgit2sharp/issues/89
	if file then return end
	local repo = repo(package)
	local ref = repo:ref_dwim'master'
	local id = repo:ref_name_to_id(ref:name())
	local commit = repo:commit(id)
	local t = commit:time()
	commit:free()
	ref:free()
	repo:free()
	return t
end or function(package, file)
	local date = git(package, 'log -1 --format=%cd --date=iso'..
		(file and ' --follow \''..file..'\'' or ''))
	date = glue.trim(date)
	local y,m,d,h,M,s = date:match'^(%d+)%-(%d+)%-(%d+) (%d+):(%d+):(%d+)'
	return os.time{year = y, month = m, day = d, hour = h, min = M, sec = s}
end)

--track_module override: find the module's loader based on its file extension
------------------------------------------------------------------------------

--modules with extensions other than Lua need a require() loader to be
--installed first. that loader is usually installed by loading another module.
local loader_modules = {dasl = 'dynasm'}

function module_loader(mod, package)
	package = package or module_package(mod)
	local path = modules(package)[mod]   ; if not path or path == true then return end
	local ext = path:match'%.(.*)$'      ; if not ext then return end
	return loader_modules[ext]
end

local track_module_ = track_module
luapower.track_module = memoize_mod_package(function(mod, package)
	package = package or module_package(mod)
	local loader_mod = module_loader(mod, package)
	return track_module_(mod, loader_mod)
end)


--track_module_platform: track requires on multiple platforms via rpc servers
------------------------------------------------------------------------------

db = nil --{platform = {package = {module = tracking_table}}}

local function dbfile()
	return powerpath'luapower_db.lua'
end

function load_db()
	if db then return end
	local dbfile = dbfile()
	db = glue.fileexists(dbfile) and assert(loadfile(dbfile))() or {}
end

function save_db()
	assert(db, 'db not loaded')
	local dbfile = dbfile()
	local tmpfile = dbfile..'.tmp' --make sure it's in the same filesystem
	glue.writefile(tmpfile, coroutine.wrap(function()
		 coroutine.yield'return '
		 pp.write(coroutine.yield, db)
	end))
	local function check(ret, err)
		if ret ~= nil then return end
		os.remove(tmpfile)
		error(err, 2)
	end
	if ffi.os == 'Windows' then
		check(os.remove(dbfile)) --no atomic replace on Windows
	end
	check(os.rename(tmpfile, dbfile)) --atomic replace on POSIX
end

function clear_db(package, platform)
	load_db()
	if not platform then
		for platform in pairs(db) do
			clear_db(package, platform)
		end
		return
	end
	if not package then
		db[platform] = nil
	else
		glue.attr(db, platform)[package] = nil
	end
end

function update_db(package, platform0)
	clear_db(package, platform0)
	local loop = require'socketloop'
	for platform in glue.sortedpairs(config'servers') do
		if not platform0 or platform == platform0 then
			loop.newthread(function()
				local lp, err
				if platform == current_platform() then
					lp = luapower
				else
					lp, err = connect(platform)
				end
				if not lp then
					--print(platform..': '..err)
					return
				end
				local data = lp.exec(function(package)
					local lp = require'luapower'
					local glue = require'glue'
					local t = {}
					if package then
						local t = glue.attr(t, package)
						for mod in pairs(lp.modules(package)) do
							t[mod] = lp.track_module(mod, package)
						end
					else
						for package in pairs(lp.installed_packages()) do
							local t = glue.attr(t, package)
							for mod in pairs(lp.modules(package)) do
								t[mod] = lp.track_module(mod, package)
							end
						end
					end
					return t
				end, package)
				if platform ~= current_platform() then
					lp.close()
				end
				glue.update(glue.attr(db, platform), data)
			end)
		end
	end
	loop.start(1)
end

function track_module_platform(mod, package, platform)
	platform = check_platform(platform)
	package = package or module_package(mod)
	load_db()
	if not (db[platform] and db[platform][package] and db[platform][package][mod]) then
		update_db(package, platform)
	end
	return db[platform] and db[platform][package] and db[platform][package][mod] or {}
end

function server_status(platform0)
	local loop = require'socketloop'
	local t = {}
	for platform in glue.sortedpairs(config'servers') do
		if not platform0 or platform == platform0 then
			loop.newthread(function()
				local lp, err = connect(platform)
				if lp then
					local os, arch = lp.osarch()
					t[platform] = {os = os, arch = arch}
	 				lp.close()
	 			else
	 				t[platform] = {err = err}
	 			end
			end)
		end
	end
	loop.start(1)
	return t
end


--module tracking breakdown
------------------------------------------------------------------------------

local empty = {}

function module_requires_loadtime(mod, package, platform)
	return track_module_platform(mod, package, platform).mdeps or empty
end

function module_load_error(mod, package, platform)
	return track_module_platform(mod, package, platform).loaderr
end

module_platforms = memoize_mod_package(function(mod, package)
	package = package or module_package(mod)
	local t = {}
	for platform in pairs(platforms(package)) do
		local err = module_load_error(mod, package, platform)
		if not err or not (err:find'platform not ' or err:find'arch not ') then
			t[platform] = true
		end
	end
	return t
end)

function module_requires_loadtime_ffi(mod, package, platform)
	return track_module_platform(mod, package, platform).ffi_deps or empty
end

function module_autoloads(mod, package, platform)
	return track_module_platform(mod, package, platform).autoloads or empty
end

module_requires_runtime = memoize(function(mod, package, platform)
	local err = module_load_error(mod, package, platform)
	--if module doesn't load, hide its runtime deps.
	if err then return {} end
	local loadtime = module_requires_loadtime(mod, package, platform)
	return filter(module_requires_parsed(mod),
			function(mod) return not loadtime or not loadtime[mod] end)
end)

module_autoloaded = memoize(function(mod, package, platform)
	local t = {}
	for _,mod in pairs(module_autoloads(mod, package, platform)) do
		t[mod] = true
	end
	return t
end)

module_requires_alltime = memoize(function(mod, package, platform)
	return glue.update({},
		module_requires_loadtime(mod, package, platform),
		module_autoloaded(mod, package, platform),
		module_requires_runtime(mod, package, platform))
end)


--indirect module dependencies
------------------------------------------------------------------------------

local function callback(cb)
	return rawget(luapower, cb) or cb
end

--given a dependency-getting function, get a module's dependencies
--recursively, as a table's keys.
function module_requires_recursive_keys_for(deps_func)
	local deps_func = callback(deps_func)
	return memoize(function(mod, package, platform)
		local t = {}
		local function add_deps(mod, package, platform)
			for dep in pairs(deps_func(mod, package, platform)) do
				if not t[dep] then --prevent cycles
					t[dep] = true
					add_deps(dep, nil, platform) --we don't know the package of indirect deps.
				end
			end
		end
		add_deps(mod, package, platform)
		return t
	end)
end

--given a dependency-getting function, get a module's dependencies
--recursively, as a tree.
function module_requires_recursive_tree_for(deps_func)
	local deps_func = callback(deps_func)
	return memoize(function(mod, package, platform)
		local function add_deps(pnode, package, platform)
			for dep in pairs(deps_func(pnode.name, package, plaform)) do
				local node = {name = dep}
				pnode.children = pnode.children or {}
				table.insert(pnode.children, node)
				add_deps(node, nil, platform)
			end
			return pnode
		end
		return add_deps({name = mod}, package, platform)
	end)
end

--given a dependency-getting function, get modules that depend on a module
--recursively, as a teble's keys.
function module_required_recursive_keys_for(deps_func)
	local deps_func = callback(deps_func)
	return memoize(function(mod, package0, platform)
		local t = {}
		package0 = package0 or module_package(mod)
		for package in pairs(installed_packages()) do
			if package ~= package0 then --not of self
				for dmod in pairs(modules(package)) do
					if deps_func(dmod, package, platform)[mod] then
						t[dmod] = true
					end
				end
			end
		end
		return t
	end)
end

module_requires_loadtime_tree = module_requires_recursive_tree_for(module_requires_loadtime)
module_requires_loadtime_all  = module_requires_recursive_keys_for(module_requires_loadtime)
module_requires_alltime_all   = module_requires_recursive_keys_for(module_requires_alltime)

--direct and indirect internal (i.e. same package) module dependencies of a module
module_requires_loadtime_int = memoize(function(mod, package, platform)
	package = package or module_package(mod)
	local internal = modules(package)
	return filter(module_requires_loadtime_all(mod, package, platform),
			function(m) return internal[m] end)
end)

--direct external module dependencies of a module and its internal dependencies
module_requires_loadtime_ext = memoize(function(mod, package, platform)
	local t = {}
	package = package or module_package(mod)
	glue.update(t, module_requires_loadtime(mod, package, platform)) --direct deps of mod
	for mod in pairs(module_requires_loadtime_int(mod, package, platform)) do --internal deps of mod
		glue.update(t, module_requires_loadtime(mod, package, platform)) --direct deps of internal deps of mod
	end
	local internal = modules(package)
	return filter(t, function(mod) return not internal[mod] end)
end)

--all modules that depend on a module
module_required_loadtime_all = module_required_recursive_keys_for(module_requires_loadtime)
module_required_alltime_all  = module_required_recursive_keys_for(module_requires_alltime)


--indirect ffi dependencies
------------------------------------------------------------------------------

--given a dependency-getting funtion, get a module's ffi dependencies.
function module_requires_ffi_for(deps_func, mod, package, platform)
	local deps_func = callback(deps_func)
	local t = glue.update({}, module_requires_loadtime_ffi(mod, package, platform))
	for mod in pairs(deps_func(mod, package, platform)) do
		glue.update(t, module_requires_loadtime_ffi(mod, nil, platform))
	end
	return t
end


--indirect package dependencies
------------------------------------------------------------------------------

--direct and indirect binary dependencies of a package
bin_deps_all = memoize_opt_package(function(package, platform)
	local t = {}
	local function add_deps(package)
		for dep in pairs(bin_deps(package, platform)) do
			if not t[dep] then
				t[dep] = true
				add_deps(dep)
			end
		end
	end
	add_deps(package)
	return t
end)

--package deps for a single module, for a module dependency-getting func.
function module_requires_packages_for(module_deps_func, mod, package, platform, add_bin_deps)
	local module_deps_func = callback(module_deps_func)
	package = package or module_package(mod)
	local deps = {}
	for mod in pairs(module_deps_func(mod, package, platform)) do
		local dep_package = modules(package)[mod] and package or module_package(mod)
		if dep_package and dep_package ~= package then
			deps[dep_package] = true
			if add_bin_deps then
				glue.update(deps, bin_deps_all(dep_package, platform))
			end
		end
	end
	if add_bin_deps then
		glue.update(deps, bin_deps_all(package, platform))
	end
	return deps
end

--combined package dependencies of all modules of a package
function package_requires_packages_for(mdeps_func, package, platform, add_bin_deps)
	mdeps_func = callback(mdeps_func)
	local pdeps = {}
	for mod in pairs(modules(package)) do
		glue.update(pdeps, module_requires_packages_for(mdeps_func, mod,
			package, platform, add_bin_deps))
	end
	return pdeps
end


--analytic info
------------------------------------------------------------------------------

--analytic info for a module
module_tags = memoize(function(package, mod)
	local mod_path = modules(package)[mod]
	return {
		lang =
			mod_path == true and 'built-in'
			or lua_module_name(mod_path) and 'Lua'
			or dasl_module_name(mod_path) and 'Lua/ASM'
			or c_module_name(mod_path) and 'C',
		demo_module = scripts(package)[mod..'_demo'] and mod..'_demo',
		test_module = scripts(package)[mod..'_test'] and mod..'_test',
	}
end)

--analytic info for a package
package_type = memoize_package(function(package)
	local has_c = csrc_dir(package) and true or false
	local has_mod = next(modules(package)) and true or false
	local has_mod_lua = false
	local has_mod_c = false
	local has_ffi = false
	for mod in pairs(modules(package)) do
		local lang = module_tags(package, mod).lang
		if lang == 'C' then
			has_mod_c = true
		else
			has_mod_lua = true
		end
		if module_requires_loadtime_all(mod, package).ffi then
			has_ffi = true
			break
		end
	end
	assert(not has_ffi or has_mod_lua) --ffi modules are written in Lua
	assert(not has_mod_c or has_c) --Lua/C modules without source?
	return
		has_ffi and 'Lua+ffi'
		or has_mod and (has_mod_c and 'Lua/C' or 'Lua')
		or has_c and 'C' or 'other'
end)

packages_cats = memoize(function()
	local t = {}
	for i,cat in ipairs(cats()) do
		for i,pkg in ipairs(cat.packages) do
			t[pkg] = cat.name
		end
	end
	return t
end)

function package_cat(pkg)
	return packages_cats()[pkg]
end

build_order = memoize(function(packages, platform)
	platform = check_platform(platform)
	local function input_packages()
		if not packages then
			return glue.update({}, installed_packages())
		end
		local t = {}
		for pkg in glue.gsplit(packages, '%s*,%s*') do
			t[pkg] = true
		end
		return t
	end
	local function dep_maps()
		local t = {}
		local function add_pkg(pkg)
			if t[pkg] then return true end --already added
			glue.assert(known_packages()[pkg], 'unknown package "%s"', pkg)
			if not build_platforms(pkg)[platform] then return end --not buildable
			glue.assert(installed_packages()[pkg], 'package not installed "%s"', pkg)
			local deps = bin_deps(pkg, platform)
			local dt = {}
			t[pkg] = dt
			for pkg in pairs(deps) do
				if add_pkg(pkg) then
					dt[pkg] = true
				end
			end
			return true --added
		end
		for pkg in pairs(input_packages()) do
			add_pkg(pkg)
		end
		return t
	end
	--build packages with zero deps first, remove them from the dep lists
	--of all other packages and from the list of packages to build,
	--and repeat, until there are no more packages.
	local t = dep_maps()
	local dt = {}
	while next(t) do
		local guard = true
		for pkg, deps in glue.sortedpairs(t) do --stabilize the list
			if not next(deps) then
				guard = false
				table.insert(dt, pkg) --build it
				t[pkg] = nil --remove it from the to-build table
				--remove it from all dep lists
				for _, deps in pairs(t) do
					deps[pkg] = nil
				end
			end
		end
		assert(not guard, 'all packages have dependencies')
	end
	return dt
end)


--consistency checks
--============================================================================

--check for the same doc in a different path.
--since all docs share the same namespace on the website, this is not allowed.
duplicate_docs = memoize(function()
	local dt = {} --{doc = package}
	local dupes = {}
	for package in pairs(installed_packages()) do
		for doc, path in pairs(docs(package)) do
			if dt[doc] then
				dupes[doc..' in '..package..' and '..dt[doc]] = true
			end
		end
	end
	return dupes
end)

--check for undocumented packages
undocumented_package = memoize_opt_package(function(package)
	local t = {}
	local docs = docs(package)
	if not docs[package] then
		t[package] = true
	end
	return t
end)

--check for csrc dir not matching package name
nonstandard_csrc_dir = memoize_opt_package(function(package)
	local t = {}
	local dir = csrc_dir(package)
	if dir and dir ~= 'csrc/'..package then
		t[dir] = true
	end
	return t
end)

--module load errors for each module of a package
load_errors = memoize_opt_package(function(package, platform)
	local errs = {}
	for mod in pairs(modules(package)) do
		local err = module_load_error(mod, package, platform)
		if err and not err:find'platform not ' and not err:find'arch not ' then
			errs[mod] = err
		end
	end
	return errs
end)


--comparison function for table.sort() for modules: sorts built-ins first.

function module_name_cmp(a, b)
	if builtin_modules[a] == builtin_modules[b] or
		luajit_builtin_modules[a] == luajit_builtin_modules[b]
	then
		--if a and be are in the same class, compare their names
		return a < b
	else
		--compare classes (std. vs non-std. module)
		return not (builtin_modules[b] or luajit_builtin_modules[b])
	end
end


--use remotely via rpc server
--============================================================================

local rpc = require'luapower_rpc'

function connect(ip, port, connect)
	local srv = config('servers')[ip]
	if srv then
		ip, port = unpack(srv)
	end
	assert(ip, 'invalid ip or platform')
	local rpc, err = rpc.connect(ip, port, connect)
	if not rpc then return nil, err end
	rpc.exec(function()
			local luapower = require'luapower'
			function exec(func, ...)
				return luapower[func](...)
			end
		end)
	local lp = {}
	function lp.close()
		if not rpc then return end --already closed
		rpc.close()
		rpc = nil
	end
	function lp.stop()
		rpc.stop()
		rpc = nil
	end
	function lp.restart()
		rpc.restart()
		rpc = nil
	end
	setmetatable(lp, {__index = function(t, k)
			return function(...)
				return rpc.exec('exec', k, ...)
			end
		end})
	return lp
end

function osarch()
	return ffi.os, ffi.arch
end

function exec(func, ...)
	return func(...)
end

function restart() error'not connected' end
function stop() error'not connected' end

return luapower

