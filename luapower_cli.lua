
--luapower command-line interface.
--Written by Cosmin Apreutesei. Public Domain.

local lp = require'luapower'
local glue = require'glue'

if ... == 'luapower_cli' then return end --loaded as module: nothing to show

--listing helpers
------------------------------------------------------------------------------

local function list_values(t)
	for i,k in ipairs(t) do
		print(k)
	end
end

local function list_keys(t, cmp)
	for k in glue.sortedpairs(t, cmp) do
		print(k)
	end
end

local function list_kv(t, cmp)
	for k,v in glue.sortedpairs(t, cmp) do
		print(string.format('%-20s %s', k, v))
	end
end

local function enum_values(t)
	return table.concat(t, ', ')
end

local function enum_keys(kt, cmp)
	local t = {}
	for k in glue.sortedpairs(kt, cmp) do
		t[#t+1] = k
	end
	return enum_values(t)
end

local function list_tree(t)
	lp.walk_tree(t, function(node, level)
		print(('  '):rep(level) .. node.name)
	end)
end

local function lister(lister)
	return function(handler, cmp)
		return function(...)
			lister(handler(...), cmp)
		end
	end
end
local values_lister = lister(list_values)
local keys_lister = lister(list_keys)
local kv_lister = lister(list_kv)
local tree_lister = lister(list_tree)

local function list_errors(title, t, lister)
	if not next(t) then return end
	local s = string.format('%s (%d)', title, glue.count(t))
	print(s)
	print(('-'):rep(#s))
	lister = lister or list_keys
	lister(t)
	print''
end

local function package_lister(handler, lister, enumerator)
	lister = lister or print
	enumerator = enumerator or glue.pass
	return function(package, ...)
		if package then
			local v = handler(package, ...)
			if v then lister(v) end
		else
			for package in glue.sortedpairs(lp.installed_packages()) do
				local v = handler(package, ...)
				if v then print(string.format('%-16s %s', package, enumerator(v))) end
			end
		end
	end
end

local function list_ctags(t)
	print(string.format('  %-20s: %s', 'clib name', t.realname))
	print(string.format('  %-20s: %s', 'clib version', t.version))
	print(string.format('  %-20s: %s', 'release url', t.url))
	print(string.format('  %-20s: %s', 'license', t.license))
	print(string.format('  %-20s: %s', 'dependencies', enum_keys(t.dependencies)))
end

local function list_mtags(package, mod)
	if not package then
		for package in pairs(lp.installed_packages()) do
			list_mtags(package)
		end
	elseif not mod or mod == '--all' then
		for mod in pairs(lp.modules(package)) do
			list_mtags(package, mod)
		end
	else
		local mt = lp.module_tags(package, mod)
		local flags = {}
		if mt.test_module then table.insert(flags, 'test') end
		if mt.demo_module then table.insert(flags, 'demo') end
		print(string.format('%-16s %-24s %-6s %-4s',
			package, mod, mt.lang, table.concat(flags, ', ')))
	end
end

local function enum_ctags(t)
	return string.format('%-24s %-16s %-16s %-36s',
		t.realname, t.version, t.license, t.url)
end

--command handlers
------------------------------------------------------------------------------

function consistency_checks(package)
	--global checks, only enabled if package is not specified
	if not package then
		list_errors('duplicate docs', lp.duplicate_docs())
	end
	--package-specific checks (they also work with no package specified)
	list_errors('undocumented packages', lp.undocumented_package(package))
	list_errors('module load errors', lp.load_errors(package), list_kv)
end

local function add_bin_deps(cmd)
	return cmd:find'^d-load' or cmd:find'^d-alltime'
end

local d_commands = {
	'd-load',        'module_requires_loadtime', 'load-time requires',
	'd-autoload',    'module_autoloaded', 'autoloaded requires',
	'd-runtime',     'module_requires_runtime', 'runtime requires',
	'd-alltime',     'module_requires_alltime', 'load-time + runtime + autoloaded requires',
	'd-load-all',    'module_requires_loadtime_all', 'load-time direct + indirect requires',
	'd-alltime-all', 'module_requires_alltime_all', 'all-time direct + indirect requires',
	'd-load-int',    'module_requires_loadtime_int', 'load-time internal (same package) requires',
	'd-load-ext',    'module_requires_loadtime_ext', 'load-time direct-external requires',
	'd-rev-load',    'module_required_loadtime_all', 'all modules that require a module at load-time',
	'd-rev-alltime', 'module_required_alltime_all', 'all modules that require a module at all-time',
}

--generate a nice markdown page for a package
local function describe_package(package)

	local llp = require'luapower' --local luapower API

	local function h(s)
		print''
		print('## '..s)
		print''
	end

	h'Overview'
	local dtags = lp.doc_tags(package, package) or {}
	print(string.format('  %-20s: %s', 'name', package))
	print(string.format('  %-20s: %s', 'tagline', dtags.tagline or ''))
	print(string.format('  %-20s: %s', 'type', lp.package_type(package)))
	print(string.format('  %-20s: %s', 'tag', lp.git_tag(package)))
	print(string.format('  %-20s: %s', 'tags', enum_values(lp.git_tags(package))))
	print(string.format('  %-20s: %s', 'version', lp.git_version(package)))
	print(string.format('  %-20s: %s', 'platforms:', enum_keys(lp.platforms(package))))
	print(string.format('  %-20s: %s', 'category:', lp.package_cat(package) or ''))

	if next(lp.modules(package)) then
		h'Modules'
		llp.walk_tree(lp.module_tree(package), function(node, level)
			local mod = node.name
			local mt = lp.module_tags(package, mod)
			local err = lp.module_load_error(mod, package)
			local deps = lp.module_requires_loadtime_ext(mod, package)
			local flags = (mt.test_module and 'T' or '') .. (mt.demo_module and 'D' or '')
			print(string.format('%-30s %-8s %-4s %s',
				('  '):rep(level) .. '  ' .. mod, mt.lang, flags,
				err and '(!) '..err or enum_keys(deps, llp.module_name_cmp)))
		end)

		h'Dependencies'
		for i=1,#d_commands,3 do
			local cmd = d_commands[i]
			local module_deps_func = d_commands[i+1]
			local add_bin_deps = add_bin_deps(cmd)
			local t = lp.package_requires_packages_for(module_deps_func,
				package, nil, add_bin_deps)
			print(string.format('  %-20s: %s', cmd, enum_keys(t)))
		end
		print(string.format('  %-20s: %s', 'd-bin',
			enum_keys(lp.bin_deps(package))))
		print(string.format('  %-20s: %s', 'd-bin-all',
			enum_keys(lp.bin_deps_all(package))))
	end

	if next(lp.scripts(package)) then
		h'Scripts'
		list_keys(lp.scripts(package))
	end

	if lp.c_tags(package) then
		h'C Lib'
		list_ctags(lp.c_tags(package))
	end

	if next(lp.docs(package)) then
		h'Docs'
		for doc, path in glue.sortedpairs(lp.docs(package)) do
			local t = lp.doc_tags(package, doc)
			print(string.format('  %-20s %s', t.title, t.tagline or ''))
		end
	end
	print''
end

--command dispatcher
------------------------------------------------------------------------------

local actions
local action_list

local function add_action(name, args, info, handler)
	local action = {name = name, args = args, info = info, handler = handler}
	actions[name] = action
	action_list[#action_list+1] = action
end

local function add_section(title)
	action_list[#action_list+1] = {title = title}
end

local function help()
	print''
	print(string.format('USAGE: luapower [-s|--server ip] [-p|--port port] <command> ...', arg[0]))
	for i,t in ipairs(action_list) do
		if t.name then
			print(string.format('   %-30s %s', t.name .. ' ' .. t.args, t.info))
		elseif t.title then
			print''
			print(t.title)
			print''
		end
	end
	print''
	print'The `package` arg defaults to the env var MULTIGIT_REPO, as set'
	print'by the `mgit` subshell, and if that is not set, it defaults to `--all`,'
	print'which means that it applies to packages.'
	print''
end

local function assert_arg(ok, ...)
	if ok then return ok,... end
	print''
	print('ERROR: '..(...))
	help()
	os.exit(1)
end

--wrapper for command handlers that take <package> as arg#1 -- provides its default value.
local function package_arg(handler, package_required, package_invalid_ok)
	return function(package, ...)
		if package == '--all' then
			package = nil
		else
			package = package or os.getenv'MULTIGIT_REPO'
		end
		assert_arg(package or not package_required, 'package required')
		assert_arg(not package or package_invalid_ok or lp.installed_packages()[package],
			'unknown package '..tostring(package))
		return handler(package, ...)
	end
end

local dmap = {}
for i=1,#d_commands,3 do
	dmap[d_commands[i]] = d_commands[i+1]
end

local function ffis_of_d_command(cmd, mod, platform)
	local module_deps_func = assert(dmap[cmd], 'invalid d-... command')
	return lp.module_requires_ffi_for(module_deps_func, mod, nil, platform, add_bin_deps(cmd))
end

local function packages_of_d_command(cmd, mod, platform)
	local module_deps_func = assert(dmap[cmd], 'invalid d-... command')
	return lp.module_requires_packages_for(module_deps_func, mod, nil, platform, add_bin_deps(cmd))
end

local function packages_of_d_command_combined(cmd, pkg, platform)
	local module_deps_func = assert(dmap[cmd], 'invalid d-... command')
	local add_bin_deps = add_bin_deps(cmd)
	return lp.package_requires_packages_for(module_deps_func, pkg, platform,
		add_bin_deps)
end

local function start_server(v, ip, port)
	local loop = require'socketloop'
	local rpc = require'luapower_rpc'
	if v ~= '-v' then
		v, ip, port = false, v, ip
	end
	rpc.verbose = v and true
	rpc.server(ip, port)
	loop.start(1)
end

local function init_actions()

	actions = {}
	action_list = {}

	add_section'HELP'
	add_action('help', '', 'this screen', help)

	add_section'PACKAGES'
	add_action('ls',          '', 'list installed packages', keys_lister(lp.installed_packages))
	add_action('ls-all',      '', 'list all known package', keys_lister(lp.known_packages))
	add_action('ls-uncloned', '', 'list not yet installed packages', keys_lister(lp.not_installed_packages))

	add_section'PACKAGE INFO'
	add_action('describe',  '<package>', 'describe a package', package_arg(describe_package, true))
	add_action('type',      '[package]', 'package type', package_arg(package_lister(lp.package_type)))
	add_action('version',   '[package]', 'current git version', package_arg(package_lister(lp.git_version)))
	add_action('tags',      '[package]', 'git tags', package_arg(package_lister(lp.git_tags, list_values, enum_values)))
	add_action('tag',       '[package]', 'current git tag', package_arg(package_lister(lp.git_tag)))
	add_action('files',     '[package]', 'tracked files', package_arg(keys_lister(lp.tracked_files)))
	add_action('docs',      '[package]', 'docs', package_arg(keys_lister(lp.docs)))
	add_action('modules',   '[package]', 'modules', package_arg(keys_lister(lp.modules)))
	add_action('scripts',   '[package]', 'scripts', package_arg(keys_lister(lp.scripts)))
	add_action('tree',      '[package]', 'module tree', package_arg(tree_lister(lp.module_tree)))
	add_action('tags',      '[package [module]]', 'module info', package_arg(list_mtags))
	add_action('platforms', '[package]', 'supported platforms', package_arg(package_lister(lp.platforms, list_keys, enum_keys)))
	add_action('ctags',     '[package]', 'C package info', package_arg(package_lister(lp.c_tags, list_ctags, enum_ctags)))

	add_section'CHECKS'
	add_action('check',        '[package]', 'run all consistency checks', package_arg(consistency_checks))
	add_action('load-errors',  '[package] [platform]', 'list module load errors', kv_lister(lp.load_errors))

	add_section'DEPENDENCIES'
	add_action('d-load-tree',    '<module>', 'load-time require tree', tree_lister(lp.module_requires_loadtime_tree))
	for i=1,#d_commands,3 do
		local func = lp[d_commands[i+1]]
		local function wrapper(mod, platform)
			return func(mod, nil, platform) --package inferred
		end
		add_action(d_commands[i], ' <module> [platform]', d_commands[i+2], keys_lister(wrapper))
	end
	add_action('d-bin',          '[package]', 'direct binary dependencies', package_arg(package_lister(lp.bin_deps, list_keys, enum_keys)))
	add_action('d-bin-all',      '[package]', 'direct+indirect binary dependencies', package_arg(package_lister(lp.bin_deps_all, list_keys, enum_keys)))
	add_action('ffi-of',         'd-... <module> [platform]', 'ffi.loads of a list of module dependencies', keys_lister(ffis_of_d_command))
	add_action('packages-of',    'd-... <module> [platform]', 'packages of a list of module dependencies', keys_lister(packages_of_d_command))
	add_action('packages-of-all','d-... [package] [platform]', 'combined package depedencies for all modules',
			function(cmd, pkg, platform)
				package_arg(package_lister(function(pkg, platform)
						return packages_of_d_command_combined(cmd, pkg, platform)
					end, list_keys, enum_keys))(pkg, platform)
			end)
	add_action('build-order',       '[package1,...] [platform]', 'build order',
		package_arg(values_lister(lp.build_order), nil, true))

	add_section'RPC'
	add_action('server',  '[-v] [ip [port]] | [platform]', 'start the RPC server', start_server)
	add_action('restart', '', 'restart a RPC server', lp.restart)
	add_action('stop',    '', 'stop a RPC server', lp.stop)
	add_action('platform','', 'report platform', function() print(lp.current_platform()) end)
	add_action('os-arch','', 'report OS and arch', function() print(lp.osarch()) end)
	add_action('server-status', '[platform]', 'show status of RPC servers',
		function(platform)
			for platform, t in glue.sortedpairs(lp.server_status()) do
				print(platform, t.os or '', t.arch or '', t.err or '')
			end
		end)

	add_section'DEPENDENCY DB'
	add_action('update-db', '[package] [platform]', 'update the dependency database',
		package_arg(function(package, platform)
			lp.update_db(package, platform)
			lp.save_db()
		end))
end

local function run(action, ...)
	action = action or 'help'
	init_actions()
	if not actions[action] then
		print''
		print('ERROR: invalid command '..action)
		print''
		return
	end
	actions[action].handler(...)
end

local t = glue.extend({}, arg)
local server, port
if t[1] == '-s' or t[1] == '--server' then
	table.remove(t, 1)
	server = table.remove(t, 1)
end
if t[1] == '-p' or t[1] == '--port' then
	table.remove(t, 1)
	port = table.remove(t, 1)
end
if server or port then
	local loop = require'socketloop'
	loop.newthread(function()
		lp = lp.connect(server, port)
		run(unpack(t))
		lp.close()
	end)
	loop.start(1)
else
	run(unpack(t))
end

