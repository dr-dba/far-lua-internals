--[[!
https://gist.github.com/johnd0e/5110ddfb3291928a7f484cd38f23ff87
https://forum.farmanager.com/viewtopic.php?t=7988
https://forum.farmanager.com/viewtopic.php?t=7521
]]

local Info = Info or package.loaded.regscript or function(...) return ... end -- luacheck: ignore 113/Info
local nfo = Info { _filename or ...,
	name		= "Lua Explorer Advanced";
	description	= "Explore Lua environment in your Far manager (+@Xer0X mod.)";
	id		= "C61B1E8D-71D4-445C-85A6-35EA1D5B6EF3";
	version		= "2.4";
	version_mod	= "1.4.5";
	author		= "jd";
	author_mod	= "Xer0X";
	url		= "http://forum.farmanager.com/viewtopic.php?f=60&t=7988";
	url_mod		= "https://github.com/dr-dba/far-lua-explorer";
	licence		= [[
based on Lua Explorer by EGez:
http://forum.farmanager.com/viewtopic.php?f=15&t=7521

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
ANY USE IS AT YOUR OWN RISK.]];
	help = function(nfo) far.Message(nfo.helpstr, ('%s v%s'):format(nfo.name,nfo.version), nil, "l") end;
	options     = {
		tables_first = true,
		ignore_case = true,
		chars = {
			['table'] = '≡',    -- ⁞≡•·÷»›►
			['function'] = '˜', -- ᶠ¨˝˜
		},
		bin_string = '#string',
	};
}
if not nfo or nfo.disabled then return end
local O = nfo.options
assert(far, 'This is LuaExplorer for Far manager')
local uuid	= win.Uuid('7646f761-8954-42ca-9cfc-e3f98a1c54d3')
nfo.helpstr	= [[

There are some keys available:

F1              Show this help
F4              Edit selected object
Del             Delete selected object
Ins             Add an object to current table
Ctrl+M          Show metatable
Ctrl+F          Show/hide functions
Ctrl+T          Toggle sort by type
Ctrl+I          Toggle ignore case

for functions:

Enter           Call function (params prompted)
F3              Show some function info
Shift+F3        Show some function info (LuaJIT required)
Alt+F4          Open function definition (if available) in editor
Ctrl+Up         Show upvalues (editable)
Ctrl+Down       Show environment (editable)


Copy to clipboard:

Ctrl+Ins        value
Ctrl+Shift+Ins  key
]]
if not Xer0X then Xer0X = { } end
local omit = {}
local brkeys = {}

-- format values for menu items and message boxes
local function fnc_val_fmt(val, mode)
	local	val_type = type(val)
	local	val_res
	if
		val_type == 'string'
	then
		val_res = val
		if not	val_res:utf8valid()
		then
			local utf_8, err = win.Utf16ToUtf8(win.MultiByteToWideChar(val, win.GetACP()));
			val_res = utf_8 or err
			val_type = O.bin_string
		end
		if	val_res:match('%z')
		or	mode == 'edit'
		then    val_res = ('%q'):format(val_res)
		elseif	mode ~= 'view' and (
				mode ~= 'list' or
				val_res == '' or
				val_res:sub(-1, -1) == " "
					)
		then	val_res = '"'..val_res..'"'
		end
	elseif
		val_type == "number"
	then
		val_res = (mode == "edit" and "0x%x --[[ %s ]]" or "0x%08x (%s)"):format(val, val)
	else
		val_res = tostring(val)
	end
	return val_res, val_type
end

-- make menu item for far.Menu(...)
local KEY_WIDTH = 30
local ITEM_FMT = ('%%-%s.%ss'):format(KEY_WIDTH, KEY_WIDTH)..'%s%-8s │%-25s'

local function fnc_make_menu_items(val_key, sval, val_type)
	local key_fmt = fnc_val_fmt(val_key, 'list')
	local border = key_fmt:len() <= KEY_WIDTH and '│' or '…'
	return {
		text	= ITEM_FMT:format(key_fmt, border, val_type, sval),
		key	= val_key,
		type	= val_type,
		checked	= O.chars[val_type]
	}
end

local function fnc_is_LE_obj(obj)
	return 2 < ( 0
		+ ("nil" ~= type(obj.obj)	and 1 or 0)
		+ ("nil" ~= type(obj.menu_idx ) and 1 or 0)
		+ ("nil" ~= type(obj.menu_item) and 1 or 0)
		+ ("nil" ~= type(obj.menu_item) and 1 or 0)
		+ ("nil" ~= type(obj.child_menu_item_txt) and 1 or 0)
		+ ("nil" ~= type(obj.child_menu_item_idx) and 1 or 0)
			)
end

-- create sorted menu items with associated keys
local function makeMenuItems(obj, obj_nav)
	local items = { }
	local item_props = { }
	-- grab all 'real' keys
	for key in pairs(obj)
	do	local sval, vt = fnc_val_fmt(obj[key], 'list')
		if not	omit[vt]
		then	table.insert(items, fnc_make_menu_items(key, sval, vt))
		end
		item_props[key] = obj[key]
	end
	--[[ Far uses some properties that in fact are functions in obj.properties
	but they logically belong to the object itself. It's all Lua magic ;) ]]
--!	if getmetatable(obj) == "access denied" then ...
	local success, props = pcall(function() return obj.properties end)
--!	if not success then far.Message(props,'Error in __index metamethod', nil, 'wl') end
	if	type(props) == 'table'
	and not rawget(obj, 'properties')
	then
		item_props.__HIDDEN_PROPS_HACK__ = true
	--!	if type(obj.properties) == 'table' and not rawget(obj, 'properties') then
		-- todo use list of APanel Area BM CmdLine Dlg Drv Editor Far Help Menu Mouse Object PPanel Panel Plugin Viewer
		for key in pairs(obj.properties)
		do	local sval, vt = fnc_val_fmt(obj[key], 'list')
			if not	omit[vt]
			then	table.insert(items, fnc_make_menu_items(key, sval, vt))
			end
			item_props[key] = obj[key]
		end
	end
	--[[!
	table.sort(items, function(v1, v2)
		return v1.text < v2.text
	end) --]]
	--[[!
	table.sort(items, function(v1, v2)
		if O.tables_first and (v1.type == 'table') ~= (v2.type == 'table')
		then	return v1.type == 'table'
		else    return v1.text < v2.text
		end
	end) ]]
-- [[
	table.sort(items, function(v1, v2)
		if
			O.tables_first and v1.type ~= v2.type
		then
			return
				v1.type == 'table' or
				v2.type ~= 'table' and v1.type < v2.type
		else
			if	O.ignore_case
			then	return v1.text:lower() < v2.text:lower()
			else    return v1.text < v2.text
			end
		end
	end) --]]
	local	obj_nav_idx
	if	obj_nav
	then	local obj_nav_val = fnc_is_LE_obj(obj_nav) and obj_nav.obj_val or obj_nav
		for key, val in pairs(items)
		do	if	val.key == obj_nav_val
			then	obj_nav_idx = key
				break
			end
		end
	end
	return items, item_props, obj_nav_idx
end

local function getres(stat, ...) return stat, stat and { ... } or (...) and tostring(...) or '', select('#', ...) end

local function checknil(t, n) for ii = 1, n do if t[ii] == nil then return true end end end

-- custom concat (applies 'tostring' to each item)
local function concat(tbl_inp, delim, pos1, pos2)
--!	assert(delim and pos1 and pos2)
	local str = pos2 > 0 and tostring(tbl_inp[pos1]) or ''
	for ii = pos1 + 1, pos2 do str = str..delim..tostring(tbl_inp[ii]) end
	return str
end

local function luaexp_prompt(Title, Prompt, Src, nArgs)
	repeat
		local	expr = far.InputBox(nil, Title:gsub('&','&&',1), Prompt, Title, Src, nil, nil, far.Flags.FIB_ENABLEEMPTY)
		if not	expr then break end
		local	f, err = loadstring('return '..expr)
		if	f
		then	local	stat, res, n = getres(pcall(f))
			if	stat
			then
				if not nArgs or (nArgs == n and not checknil(res, n))
				then	return res, n, expr
				else	err = ([[
%d argument(s) required

  Expression entered: %q
  Evaluated as %d arg(s): %s]]):format(nArgs,expr,n,concat(res,',',1,n))
				end
			else
				err = res
			end
		end
		far.Message(err, 'Error', nil, 'wl')
	until false
end

-- edit or remove object at obj[key]
local function editValue(obj, key, title, del)
	if	del
	then
		local message = ('%s is a %s, do you want to remove it?')
			:format(fnc_val_fmt(key), type(obj[key]):upper())
		if 1 == far.Message(message, 'REMOVE: '..title, ';YesNo', 'w')
		then	obj[key] = nil
		end
	else
		local v, t = fnc_val_fmt(obj[key], 'edit')
		if t == 'table' or t == 'function' then	v = ''	end
		local	prompt = ('%s is a %s, type new value as Lua code'):format(fnc_val_fmt(key), t:upper())
		local	res = luaexp_prompt('EDIT: '..title, prompt, v, 1)
		if	res
		then	if	t == O.bin_string
			then	res[1] = win.WideCharToMultiByte(win.Utf8ToUtf16(res[1]), win.GetACP())
			end
			obj[key] = res[1]
		end
	end
end

-- add new element to obj
local function insertValue(obj, title)
	local	res = luaexp_prompt('INSERT: '..title, 'type the key and value comma separated as Lua code', nil, 2)
	if	res then obj[res[1]] = res[2] end
end

local function getfParamsNames(f)
	-- check _VERSION > "Lua 5.1"
	if not jit then	return '...' end
	local info = debug.getinfo(f)
	local params = {}
	for ii = 1, info.nparams or 1000
	do	params[ii] = debug.getlocal(f, ii) or ("<%i>"):format(ii) -- C?
	end
	if info.isvararg then params[#params + 1] = '...' end
	local paramstr = #params > 0 and table.concat(params,', ') or '<none>'
	return paramstr, params
end

if not	_G.Xer0X
then	_G.Xer0X = { }
end
if not	_G.Xer0X.tbl_explorer_reopen_chain
then	_G.Xer0X.tbl_explorer_reopen_chain = { }
end
local tbl_reopen_paths = Xer0X.tbl_explorer_reopen_chain

-- show a menu whose items are associated with the members of given object
local function process(obj, title, action, obj_root, tbl_open_path)
	local tbl_ReOp_path, is_obj_root
	if	obj_root
	then	tbl_ReOp_path = tbl_reopen_paths[obj_root]
	else	tbl_ReOp_path = tbl_reopen_paths[obj]
		is_obj_root = true
		obj_root = obj
		if not	tbl_open_path
		then	tbl_open_path = { }
		end
		if not	tbl_ReOp_path
		then	tbl_ReOp_path = { }
			tbl_reopen_paths[obj] = tbl_ReOp_path
		elseif	#tbl_open_path > 0
		then	for ii = 1, #tbl_open_path, -1
			do	local obj_path_item = table.remove(tbl_ReOp_path, 1)
				if type(obj_path_item) == nil then break end
				if not	fnc_is_LE_obj(tbl_open_path[ii])
				and	obj_path_item.menu_item.key == tbl_open_path[ii]
				then	local obj_new = { }
					for ii_obj_key, ii_obj_val in pairs(obj_path_item)
					do obj_new[ii_obj_key] = ii_obj_val
					end
					tbl_open_path[ii] = obj_new
				end
			end
			while #tbl_ReOp_path > 0 do tbl_ReOp_path[#tbl_ReOp_path] = nil end
		else	while #tbl_ReOp_path > 0
			do table.insert(tbl_open_path, table.remove(tbl_ReOp_path, 1))
			end
		end
	end
	local tbl_cur_obj = is_obj_root and tbl_ReOp_path or tbl_ReOp_path[#tbl_ReOp_path]
	title = type(title) == "string" and title or ''
	--! here you can insert your custom actions
	if action and brkeys[action] then brkeys[action]({ obj }, 1, title); return end
	local	mprops = {
		Id = uuid,
		Bottom = 'F1, F3, F4, Del, Ctrl+M',
		Flags = {
			FMENU_SHOWAMPERSAND = 1,
			FMENU_WRAPMODE = 1
		}
	}
	local	menu_item, menu_idx, obj_ret, obj_rem, obj_nav
	local	obj_type = type(obj)
	--[[ some member types, need specific behavior:
	* tables are submenus
	* functions can be called --]]
	if	obj_type == 'function'
	then
		local args, n, expr = luaexp_prompt(
			'CALL:'..title,
			('arguments: %s (type as Lua code or leave empty)')
				:format(getfParamsNames(obj))
					)
		if not args then return end
		-- overwrite the function object with its return values
		local	stat, res = getres(pcall(obj, unpack(args, 1, n)))
		if not	stat
		then	far.Message(
				('%s\n  CALL: %s (%s)\n  argument(s): %d'..(n > 0 and ', evaluated as: %s' or ''))
					:format(res, title, expr, n, concat(args, ',', 1, n)),
				'Error', nil, 'wl'
			)
			return
		end
		obj = res
		title = ('%s(%s)'):format(title, expr)
	-- other values are simply displayed in a message box
	elseif	obj_type ~= 'table'
	then	local value = fnc_val_fmt(obj, 'view')
		far.Message(value, title:gsub('&', '&&', 1), nil, value:match('\n') and 'l' or '')
		return
	end
	-- show this menu level again after each return from a submenu/function call ...
	repeat
		local menu_items, item_props, obj_nav_idx = makeMenuItems(obj, tbl_open_path and tbl_open_path[1])
		mprops.Title = title..' ('..#menu_items..')'..(omit['function'] and '*' or '')
		mprops.SelectIndex = obj_nav_idx or tbl_cur_obj.child_menu_item_idx
		tbl_cur_obj.obj_props = item_props
		if	tbl_open_path
		and 	#tbl_open_path > 0
		then	menu_item= tbl_open_path[1].menu_item or obj_nav_idx and menu_items[obj_nav_idx]
			menu_idx = tbl_open_path[1].menu_idx  or obj_nav_idx
			obj_nav = table.remove(tbl_open_path, 1)
			if not	menu_item
			then	--[[ the object changed!
				it means that the old name dissapeared ]]
				far.Message(string.format("The key [%s] was removed", obj_nav), title, nil, "wl")
				obj_ret = "back"
			end
		else
		--	mprops.SelectIndex = mprops.SelectIndex or tbl_cur_obj.child_menu_item_idx
			menu_item, menu_idx = far.Menu(mprops, menu_items, brkeys)
		end
		if	menu_idx
		then	mprops.SelectIndex = menu_idx
			tbl_cur_obj.child_menu_item_idx = menu_idx
			tbl_cur_obj.child_menu_item_txt = menu_item.text
		end
		-- show submenu/call function ...
		if	menu_item
		then
			if	menu_item.name == "goBack"
			then
				obj_ret = "back"
			else
				local obj_key_child = menu_item.key or menu_idx > 0 and menu_items[menu_idx].key
				local title_child = (title ~= '' and title..'.' or title)..tostring(obj_key_child)
				if	menu_item.key ~= nil
				then	local obj_child = obj[obj_key_child]
					if type(obj_child) ~= "nil"
					then	table.insert(tbl_ReOp_path, {
							obj_val  = obj_child,
							obj_key  = obj_key_child,
							menu_idx = menu_idx,
							menu_item= menu_item,
							child_menu_item_txt = obj_nav and obj_nav.child_menu_item_txt,
							child_menu_item_idx = obj_nav and obj_nav.child_menu_item_idx,
						})
						obj_ret = process(obj_child, title_child, nil, obj_root, tbl_open_path)
						if	obj_ret ~= "exit"
						then	obj_rem = table.remove(tbl_ReOp_path)
						end
					end
				elseif	menu_item.action
				then	if menu_item.action(obj, obj_key_child, title_child, item_props) == "break" then return end
				end
			end
		end
		if not	menu_item
		and not obj_ret
		then	obj_ret = "exit"
		end
	-- until the user is bored and goes back ;)
	until	obj_ret == "exit" or
		obj_ret == "back"
	if	obj_ret == "back"
	then	--[[ no back-propagation allowed!
		so that the one back will do only one backstep:]]
		obj_ret = nil
	end
	return	obj_ret -- or not menu_item and "exit"
end

local function fnc_upvals_collect(fnc_inp, num_vals)
	local	tbl_upvals = {}
	local	ii_key, ii_val
	-- n: debug.getinfo(f).nups
	for	ii = 1, num_vals or 1000
	do	ii_key, ii_val = debug.getupvalue(fnc_inp, ii)
		if not ii_key then num_vals = ii - 1; break end
		tbl_upvals[ii_key] = ii_val
	end
	return tbl_upvals, num_vals
end

local function syncUpvalues(f, t, n)
	-- n: debug.getinfo(f).nups
	for i = (n or -1), (n and 1 or -1000), -1
	do
		local	k, v = debug.getupvalue(f, i)
		if not	k then break end
		if	t[k] ~= v
		then	assert(k == debug.setupvalue (f, i, t[k]))
		end
	end
end

brkeys = {
	{ BreakKey = 'F9', name = 'registry',
		action = function(info) process(debug.getregistry(), 'debug.getregistry:') end;},
	{ BreakKey = 'Ctrl+Insert',
		action = function(obj, key)
			-- todo: escape slashes etc
			local copy_val, copy_type = fnc_val_fmt(obj[key])
			far.CopyToClipboard(copy_val)
		end},
	{ BreakKey = 'CtrlShift+Insert',
		action = function(obj, key)
			local copy_val, copy_type = fnc_val_fmt(key, 'list')
			far.CopyToClipboard(copy_val)
		end},
	{ BreakKey = 'CtrlAlt+Insert',
		action = function(obj, key, kpath)
			local copy_val, copy_type = fnc_val_fmt(key, 'list')
			far.CopyToClipboard(kpath:gsub('^_G%.','')..copy_val)
		end},
	{ BreakKey = 'Ctrl+Up',	name = 'upvalues',
		action = function(obj, key, kpath)
-- ###
local	fnc1 = obj[key]
if	type(fnc1) == 'function'
then	local	dbg_info = debug.getinfo(fnc1)
	if	dbg_info.what ~= 'C'
	or	true -- todo
	then	local	tbl_upvals, num_vals = fnc_upvals_collect(fnc1)
		if	num_vals > 0
		then	process(tbl_upvals, 'upvalues: '..kpath)
			syncUpvalues(fnc1, tbl_upvals, num_vals)
		else	far.Message(
				"No upvalues",
				dbg_info.name
					or key ~= "func" and key
					or string.gsub(string.gsub(kpath, "debug.getinfo: ", ""), ".func$", ""),
				nil,
				"w"
			)
		end
	end
end
-- @@@
		end; },
	{ BreakKey = 'Ctrl+Down', name = 'env',
		action = function(obj, key, kpath)
-- ###
local	f = obj[key];
local	t = type(f)
if	t == 'function'
or	t == 'userdata'
or	t == 'thread'
then
	local env = debug.getfenv(f)
	local env_is_glob = env == _G
        process(env, 'getfenv: '..kpath..(env_is_glob and " (_G)" or ""))
	--[[
	local dlg_res = far.Message('Show global environment?', '_G', ';OkCancel')
	if (env ~= _G or dlg_res == 1) and env and next(env)
	then process(env, 'getfenv: '..kpath)
	end --]]
end
-- @@@
		end;},
	{ BreakKey = 'Ctrl+Right', name = 'params',
		action = function(obj, key, kpath)
-- ###
local f = obj[key]
if type(f) == 'function'
then
	local	args, t = getfParamsNames(f)
	if	args:len() > 0
	then	process(t, 'params (f): '..kpath)
		local name = debug.getinfo(f).name
	--	far.Message(('%s (%s)'):format(name or kpath,args), 'params')
	end
end
-- @@@
		end;},
	{ BreakKey = 'Alt+F4', name = 'edit',
		action = function(obj, key, kpath, props)
-- ###
local	fnc_test = obj[key]
local	test_is_func = type(fnc_test) == 'function'
local	test_is_info =
	not	props.__HIDDEN_PROPS_HACK__
	and	obj.linedefined
	and	obj.source
	and type(obj.func) == 'function'
if 	test_is_func
or	test_is_info
then
	local	fnc_targ = test_is_func and fnc_test or obj.func
	local	dbg_info = test_is_info and obj or debug.getinfo(fnc_targ, 'Slun')
	--[[ @Xer0X:do not gives current line:
	debug.getinfo(fnc_targ, 'Slun') --]]
	local	filename =
		dbg_info.source:match("^@(.+)$")
	local	fileline =
		dbg_info.currentline and
		dbg_info.currentline > 0 and
		dbg_info.currentline
		or
		dbg_info.linedefined and
		dbg_info.linedefined > 0 and
		dbg_info.linedefined
	if	filename
	then	editor.Editor(filename, nil, nil, nil, nil, nil, nil, fileline)
	end
end
-- @@@
		end;},
	{ BreakKey = 'F3', name = 'info',
		action = function(obj, key, kpath)
-- ###
local	f = obj[key]
if	type(f) == 'function'
then	process(debug.getinfo(f), 'debug.getinfo: '..kpath)
elseif	type(f) == 'thread'
then	far.Message(debug.traceback(f, "level 0", 0):gsub('\n\t','\n   '), 'debug.traceback: '..kpath, nil, "l")
--	far.Show('debug.traceback: '..kpath..debug.traceback(f, ", level 0", 0))
end
-- @@@
		end; },
	{ BreakKey = 'F4',
		action = function(obj, key, kpath) return key ~= nil and editValue(obj, key, kpath) end },
	{ BreakKey = 'Ctrl+F',
		action = function()	omit['function']= not omit['function']	end },
	{ BreakKey = 'Ctrl+T',
		action = function()	O.tables_first	= not O.tables_first	end },
	{ BreakKey = 'Ctrl+I',
		action = function()	O.ignore_case	= not O.ignore_case	end },
	{ BreakKey = 'Ctrl+M', name = 'mt',
		action = function(obj, key, kpath)
-- ###
local mt = key ~= nil and debug.getmetatable(obj[key])
return mt and process(mt, 'METATABLE: '..kpath)
-- @@@
		end; },
	{ BreakKey = 'DELETE',	action = function(obj, key, kpath) return key ~= nil and editValue(obj, key, kpath, true) end },
	{ BreakKey = 'INSERT',	action = function(obj, key, kpath) insertValue(obj, kpath:sub(1, -(#tostring(key) + 2))) end },
	{ BreakKey = 'F1',	action = function() nfo:help() end},
	{ BreakKey = nil,	name = 'addBrKeys',
		action = function(obj, key)
-- ###
local	addbrkeys = obj[key]
for	ii = 1, #addbrkeys
do	local bk = addbrkeys[ii]
	local BreakKey = bk.BreakKey
	local pos
	for jj = 1, #brkeys do if brkeys[jj].BreakKey == BreakKey then pos = jj; break end end
	if	pos
	then	brkeys[pos] = bk
	else	table.insert(brkeys, bk)
		if bk.name then	brkeys[bk.name] = bk.action end
	end
end
return "break"
-- @@@
		end; },
	{ BreakKey = 'BS', name = "goBack", action = function() return "goBack" end }
}

--[[ if LuaJIT is used,
maybe we can show some more function info]]
if	jit
then    local funcinfo = require('jit.util').funcinfo
	table.insert(brkeys, {
		BreakKey = 'Shift+F3',
		action = function(obj, key, kpath)
			if	key ~= nil
			and	type(obj[key]) == 'function'
			then
				local name_x = debug.getinfo(obj[key], "n").name
				far.Message(name_x)
				process(funcinfo(obj[key]), 'jit.util.funcinfo: '..kpath)
			end
		end,
		name = 'jitinfo'
	})
end

for ii = 1, #brkeys
do	local bk = brkeys[ii];
	if bk.name then	brkeys[bk.name] = bk.action end
end

nfo.execute = function() process(_G, '') --[[ require("le")(_G,'_G') ]] end

if	Macro
then
-- ###
Macro { description = "Lua Explorer (Advanced)";
	area = "Common"; key = "RCtrlShiftF12";
	action = nfo.execute
}
-- @@@
elseif	_filename
then	process(_G, '')
else	return process
-- if ... == "le" then
end

--[[ it's possible to call via lua:, e.g. from user menu:
lua:dofile(win.GetEnv("FARPROFILE")..[[\Macros\scripts\le.lua] ])(_G,'_G')
lua:require("le")(_G,'_G')
--]]
-- @@@@@
