--[[
if true then return end --]]

--[[		(c) Xer0X
	"Православие или Смерть!" group

	Tools for "introspection",
	aka live code analysis.
]]

local Xer0X = require("Lib-Common-@Xer0X")
local fnc_norm_script_path	= Xer0X.fnc_norm_script_path
local fnc_str_trim		= Xer0X.fnc_str_trim1

Xer0X.fnc_definition_parse = function(line)
	assert(type(line) == "string")
	local match
	match = line:match("^%s*function%s*([%w_.]+[.]?[%w_]+)") 
	if match then return match end
	match = line:match("^%s*local%s+function%s*([%w_]+)")
	if match then return match end
	match = line:match("^%s*local%s+([%w_]+)%s*=%s*function")
	if match then return match end
	match = line:match("^%s*([%w_.]+[.]?[%w_]+)%s*=%s*function")
	if match then return match end
	match = line:match("%s*function%s*%(")
	if match then return "(anonymous)" end
	return "(anonymous)"
end -- fnc_definition_parse

--[[ Private:
Tries to guess a function's name when the debug info structure does not have it.
It parses either the file or the string where the function is defined.
Returns '?' if the line where the function is defined is not found
--]]
Xer0X.fnc_func_name_read = function(dbg_info)
	local	line_str, line_def, line_cur
	local 	tbl_src_code = { }
	if	type(dbg_info.source) == "string"
	and	dbg_info.source:sub(1, 1) == "@"
	then
		local	fileH, err = io.open(dbg_info.source:sub(2), "r")
		if not	fileH
		then	return "?"
		end
		for	line_num = 1, dbg_info.lastlinedefined
		do 	line_str = fileH:read("*l")
			if not	line_str then break end
			if	line_num == dbg_info.linedefined
			then	line_def = line_str
			end
			if	line_num == dbg_info.currentline
			then	line_cur = line_str
			end
			if	line_num >= dbg_info.linedefined and
				line_num <= dbg_info.lastlinedefined
			then	tbl_src_code[#tbl_src_code + 1] = line_str
			end
		end
		fileH:close()
	else
		local	line_num = 0
		for	line_str in string.gmatch(dbg_info.source, "([^\n]+)\n-")
		do	line_num = line_num + 1
			if	line_num == dbg_info.linedefined
			then	line_def = line_str
			end
			if	line_num == dbg_info.currentline
			then	line_cur = line_str
			end
			if	line_num >= dbg_info.linedefined and
				line_num <= dbg_info.lastlinedefined
			then	tbl_src_code[#tbl_src_code + 1] = line_str
			end
		end
	end
	return
		line_def and Xer0X.fnc_definition_parse(line_def) or "?",
		table.concat(tbl_src_code, string.char(10)..string.char(13)),
		line_cur
end -- fnc_func_name_read


Xer0X.fnc_menu_dbg_inf_key_weight = function (menu_item_key)
	if	menu_item_key:match("^ERROR$")
	then	return 1
	elseif	menu_item_key:match("^name$")
	then	return 2
	elseif	menu_item_key:match("^what$")
	then	return 3
	elseif	menu_item_key:match("^namewhat$")
	then	return 4
	elseif	menu_item_key:match("^nups$")
	then	return 5
	elseif	menu_item_key:match("^nparams$")
	then	return 6
	elseif	menu_item_key:match("^isvararg$")
	then	return 7
	elseif	menu_item_key:match("^linedefined$")
	then	return 8
	elseif	menu_item_key:match("^currentline_txt$")
	then	return 9
	elseif	menu_item_key:match("^currentline$")
	then	return 10
	elseif	menu_item_key:match("^lastlinedefined$")
	then	return 11
	elseif	menu_item_key:match("^source_code$")
	then	return 12
	elseif	menu_item_key:match("^short_src$")
	then	return 13
	elseif	menu_item_key:match("^source$")
	then	return 14
	elseif	menu_item_key:match("^func$")
	then	return 15
	end
end

Xer0X.fnc_exec_time_info_read = function(src_thr, src_lev, tbl_opts, tbl_upvals, tbl_params, tbl_vararg, tbl_locals)
	if	type(src_thr) ~= "thread"
	and	type(src_thr) ~= "nil"
	then	src_thr, src_lev, tbl_opts, mode_upvals, tbl_params, tbl_vararg, tbl_locals = nil,
		src_thr, src_lev, tbl_opts, mode_upvals, tbl_params, tbl_vararg, tbl_locals
	end
	local opts = tbl_opts
	local show_info = opts.show_info
	local with_nums = opts.with_nums
	local mode_upvals=opts.mode_upvals or 2
	local mode_vararg=opts.mode_vararg or 2
	local mode_locals=opts.mode_locals or 2
	local mode_params=opts.mode_params or 2
	if not tbl_upvals then tbl_upvals = { } end
	if not tbl_vararg then tbl_vararg = { } end
	if not tbl_locals then tbl_locals = { } end
	if not tbl_params then tbl_params = { } end
--	src_lev = (src_lev or 1) + (src_thr and 0 or 1)
	local dbg_info, LoadedMacros, mcr_src, mcr_inf, dbg_props
	local is_same_thr = not src_thr or coroutine.running() == src_thr
--	if is_same_thr then src_thr = nil end
	if	is_same_thr
	then	dbg_info = debug.getinfo(src_lev, dbg_props) -- "nSlfu" ?
	else	dbg_info = debug.getinfo(src_thr, src_lev, dbg_props)
		if	src_lev == 0
		and	opts.orig_err_msg_line
		and	opts.orig_err_msg_line	>= 0
		and	dbg_info.linedefined	>= 0
		and	dbg_info.lastlinedefined>= 0
		and	dbg_info.currentline	==-1
		then	dbg_info.currentline = opts.orig_err_msg_line
		end
	end
	if	dbg_info
	then	local fnc_name, fnc_code, curr_line = Xer0X.fnc_func_name_read(dbg_info)
		if not	dbg_info.name
		then	dbg_info.name = fnc_name
                end
		dbg_info.currentline_txt = fnc_str_trim(curr_line)
		dbg_info.source_code = fnc_code
		mcr_src = dbg_info.source:sub(2)
		mcr_inf = win.GetFileInfo(mcr_src)
		dbg_info._LEX_FNC_KEY_WEIGHT = Xer0X.fnc_menu_dbg_inf_key_weight
		if	dbg_info.namewhat == ""
		then	dbg_info.namewhat = nil
		end
		if	dbg_info.linedefined == -1
		then	dbg_info.linedefined = nil
		end
		if	dbg_info.currentline == -1
		then	dbg_info.currentline = nil
		end
		if	dbg_info.lastlinedefined == -1
		then	dbg_info.lastlinedefined = nil
		end
		if	mode_upvals > 0
		-- check for func as it may be nil for tail calls:
		and	dbg_info.func
		then
			local tbl_i2n, tbl_n2i, tbl_raw = { }, { }, { }
			for	ii = 1, dbg_info.nups
			do	local var_N, var_V = debug.getupvalue(dbg_info.func, ii)
				if	var_V == nil
				then	var_V = "<NIL>"
				end
				tbl_upvals[(with_nums and ii..".) " or "")..var_N] = var_V
				tbl_raw[var_N]	= var_V
				tbl_n2i[var_N]	= ii
				tbl_i2n[ii]	= var_N
			end
			tbl_upvals._LEX_VAR_MAP = {
				raw = tbl_raw,
				n2i = tbl_n2i,
				i2n = tbl_i2n,
				cnt = dbg_info.nups
			}
			tbl_upvals._LEX_FNC_KEY_WEIGHT = function(menu_item_key) return tbl_n2i[menu_item_key] end
		end
		if	mode_vararg > 0
		then
			tbl_vararg._LEX_VARARG_VALS = { }
			local	ii = 0
			while	true
			do
				ii = ii + 1
				local	var_N, var_V
				if	is_same_thr
				then	var_N, var_V = debug.getlocal(src_lev, -ii)
				else	var_N, var_V = debug.getlocal(src_thr, src_lev, -ii)
				end
			        if not	var_N then break end
				if	var_V == nil
				then	var_V = "<NIL>"
				end
				if not	var_N
				then	break
				end
				tbl_vararg[var_N:gsub("%)$", " "..ii..")")] = var_V
				table.insert(tbl_vararg._LEX_VARARG_VALS, var_V)
			end
		end
		if	mode_locals > 0
		then
			local tbl_i2n, tbl_n2i, tbl_raw, tbl_arg = { }, { }, { }, { }
			local	ii = 0
			local	tbl_mult_N = { }
			while	true
			do	ii = ii + 1
				local	var_N, var_V
				if	is_same_thr
				then	var_N, var_V = debug.getlocal(src_lev, ii)
				else	var_N, var_V = debug.getlocal(src_thr, src_lev, ii)
				end
			        if not	var_N then break end
				if	var_V == nil
				then	var_V = "<NIL>"
				end
				local	var_M = true and (
						tbl_locals[var_N] or
						tbl_mult_N[var_N]
							)
				if	var_M
				then    -- already exists with the same name
					local var_C = (tbl_mult_N[var_N] or 1) + 1
					tbl_mult_N[var_N] = var_C
					if	var_C == 2
					then	local varNN = string.format("%s (#1)", var_N)
						local var_i =
						tbl_n2i[var_N]
						tbl_i2n[var_i] = varNN
						tbl_raw[var_N],
						tbl_raw[varNN] = nil,
						tbl_raw[var_N]
						tbl_n2i[var_N],
						tbl_n2i[varNN] = nil,
						tbl_n2i[var_N]
						tbl_locals[var_N],
						tbl_locals[varNN] = nil,
						tbl_locals[var_N]
					end
					var_N = string.format("%s (#%s)", var_N, var_C)
				end
				if	band(mode_locals, 2) == 2
				then	tbl_locals[(with_nums and ii..".) " or "")..var_N] = var_V
				end
				if	band(mode_params, 2) == 2
				and	ii <= dbg_info.nparams
				then	tbl_params[(with_nums and ii..".) " or "")..var_N] = var_V
				end
				tbl_raw[var_N]	= var_V
				tbl_n2i[var_N]	= ii
				tbl_i2n[ii]	= var_N
			end
			tbl_locals._LEX_VAR_MAP = {
				arg = tbl_arg,
				raw = tbl_raw,
				n2i = tbl_n2i,
				i2n = tbl_i2n,
				cnt = ii - 1 ,
			}
			tbl_locals._LEX_FNC_KEY_WEIGHT = function(menu_item_key) return tbl_n2i[menu_item_key] end
			tbl_params._LEX_FNC_KEY_WEIGHT = function(menu_item_key) return tbl_n2i[menu_item_key] end
		end
	elseif	type(src_lev) == "string"
	then	mcr_inf = win.GetFileInfo(src_lev)
		if mcr_inf then mcr_src = src_lev end
	end
	if mcr_src then mcr_src = far.ConvertPath(mcr_src, 1) end
	return mcr_src, mcr_inf, mcr_inf and mcr_inf.LastWriteTime, tbl_upvals, tbl_params, tbl_vararg, tbl_locals, dbg_info
end -- fnc_exec_time_info_read


local all_the_stuff = { }
for ii = 0, 1000 
do
	local mcr_src, mcr_inf, LastWriteTime, tbl_upvals, tbl_params, tbl_vararg, tbl_locals, dbg_info
		= Xer0X.fnc_exec_time_info_read(ii, {
			mode_upvals = 2,
			mode_params = 2,
			mode_vararg = 2,
			mode_locals = 2,
				})
	if not dbg_info then break end
	all_the_stuff[ii + 1] = {
		mcr_src = mcr_src,
		mcr_inf = mcr_inf,
		LastWriteTime = LastWriteTime,
		tbl_upvals = tbl_upvals,
		tbl_params = tbl_params,
		tbl_vararg = tbl_vararg,
		tbl_locals = tbl_locals,
		dbg_info = dbg_info
	}
	if	tbl_upvals.LoadedMacros
	then    Xer0X.utils	=	tbl_upvals
		Xer0X.utils_local =	tbl_locals
	elseif	tbl_upvals.KeyMacro
	then	Xer0X.key_mcr_inf =	tbl_upvals
		Xer0X.key_mcr_inf_loc = tbl_locals
	end
end
_G.Xer0X.internals = all_the_stuff
local tbl_macrolist_marks = { }
local tbl_mcr_items_marks = { }

Xer0X.fnc_macrolist_find = function(mark_id)
	mark_id = mark_id and ":"..mark_id or ":?"
	local ii_from, all_the_stuff
	local	tbl_mcr_lst_upv,
		tbl_mcr_lst_loc,
		tbl_mcr_items_U,
		tbl_mcr_items_L
	local tbl_mcr_lst_upv_idx = tbl_macrolist_marks["UPV"..mark_id]
	local tbl_mcr_lst_loc_idx = tbl_macrolist_marks["LOC"..mark_id]
	local tbl_mcr_items_L_idx = tbl_mcr_items_marks["LOC"..mark_id]
	local found =
		tbl_mcr_lst_upv_idx or
		tbl_mcr_lst_loc_idx
	local	var_key, var_val, dbg_inf
	if	tbl_mcr_lst_loc_idx
	then	dbg_inf = debug.getinfo(tbl_mcr_lst_loc_idx - 1)
		if	dbg_inf
		then	var_key, var_val = debug.getlocal(tbl_mcr_lst_loc_idx - 1, Xer0X.env_mcr_lst_loc._LEX_VAR_MAP.n2i.macrolist)
		end
		if	var_key == "macrolist"
		then	tbl_mcr_lst_loc = var_val
			found = true
		else	found = false
		end
		if	found
		then	var_key, var_val = debug.getlocal(tbl_mcr_items_L_idx - 1, Xer0X.env_mcr_items_L._LEX_VAR_MAP.n2i.menuitems)
			if	var_key == "menuitems"
			then	tbl_mcr_items_L = var_val
				goto end_of_proc
			else	found = false
			end
		end
	end
	ii_from = found and (tbl_mcr_lst_loc_idx or tbl_mcr_lst_upv_idx) or 1
	all_the_stuff = { }
	for ii = ii_from, 1000 do
		local mcr_src, mcr_inf, LastWriteTime, tbl_upvals, tbl_params, tbl_vararg, tbl_locals, dbg_inf
			= Xer0X.fnc_exec_time_info_read(nil, ii, { mode_upvals = 2, mode_params = 2, mode_locals = 2 })
		if not dbg_inf then break end
		all_the_stuff[ii] = { mcr_src = mcr_src, mcr_inf = mcr_inf, LastWriteTime = LastWriteTime, tbl_locals = tbl_locals, tbl_upvals = tbl_upvals, dbg_inf = dbg_inf }
		if	tbl_upvals.macrolist
		then
			Xer0X.env_mcr_lst_upv = tbl_upvals
			Xer0X.env_mcr_lst_upv.LEVEL = ii
			Xer0X.env_mcr_lst_upv_loc = tbl_locals
			Xer0X.env_mcr_lst_upv_idx = ii
			tbl_mcr_lst_upv = tbl_upvals.macrolist
			tbl_macrolist_marks["UPV"..mark_id] = ii
			found = true
		end
		if	tbl_locals.macrolist
		then
			Xer0X.env_mcr_lst_loc = tbl_locals
			Xer0X.env_mcr_lst_loc_upv = tbl_upvalues
			Xer0X.env_mcr_lst_loc.LEVEL = ii
			Xer0X.env_mcr_lst_loc_idx = ii
			tbl_mcr_lst_loc = tbl_locals.macrolist
			tbl_macrolist_marks["LOC"..mark_id] = ii
			found = true
		end
		if	tbl_locals.menuitems
		then
			Xer0X.env_mcr_items_L = tbl_locals
			Xer0X.env_mcr_items_L_upv = tbl_upvalues
			Xer0X.env_mcr_items_L.LEVEL = ii
			Xer0X.env_mcr_items_L_idx = ii
			tbl_mcr_items_L = tbl_locals.menuitems
			tbl_mcr_items_marks["LOC"..mark_id] = ii
		end
	end
	if	found
	then	Xer0X.env_mcr_lst_full_stack = all_the_stuff
	end
	::end_of_proc::
	return found, tbl_mcr_lst_upv, tbl_mcr_lst_loc, tbl_mcr_items_L
end -- fnc_macrolist_find

Xer0X.fnc_mcr_src_tbl_clean = function(mcr_src, tbl_mcr)
	mcr_src = string.lower(mcr_src)
	local tbl_rebind_guids = Xer0X.ReBind and Xer0X.ReBind.GUIDs
	local mcr_cnt = #tbl_mcr
	local mcr_rem = 0
	local ii_mcr, ii_skip, ii_guid
	for ii = 1, mcr_cnt 
	do	ii_mcr = tbl_mcr[ii]
		if 	ii_mcr
		and	type(ii_mcr) == "table"
		then	ii_skip = 0
			if	-- !! we may have macros without FileName property
				ii_mcr.FileName
			and	mcr_src == string.lower(ii_mcr.FileName)
			then	mcr_rem = mcr_rem + 1
				if	tbl_rebind_guids
				then	tbl_rebind_guids[ii_mcr.id or ii_mcr.guid] = nil
				end
				ii_skip = 1
			end
			if	mcr_rem > 0
			then	tbl_mcr[ii - mcr_rem + ii_skip] = tbl_mcr[ii]
				tbl_mcr[ii] = nil
			end
		end
	end
	for	ii, ii_mcr in pairs(tbl_mcr)
	do	if	type(ii_mcr) == "table"
		and	mcr_src == string.lower(ii_mcr.FileName or "<NO-FILE-SOURCE-FOR-THIS-MACRO>")
		then	if	tbl_rebind_guids
			then	ii_guid = tbl_mcr[ii].id or tbl_mcr[ii].guid
				if	ii_guid
				and	tbl_rebind_guids[ii_guid]
				then	tbl_rebind_guids[ii_guid] = nil
				end
			end
			tbl_mcr[ii] = nil
		end
	end
end

Xer0X.fnc_mcr_src_agg_clean = function(mcr_src, tbl_agg)
	mcr_src = string.lower(mcr_src)
	for	ii_one, tbl_one in pairs(tbl_agg)
	do	for	ii_mcr, tbl_mcr in pairs(tbl_one)
		do      if tbl_mcr.FileName
			then	Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_one)
				break
			else 	Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_mcr)
			end
		end
		for	ii_mcr = 1, #tbl_one
		do      if not tbl_one[ii_mcr].FileName then
				Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_one[ii_mcr])
			end
		end
	end
	for	ii_agg = 1, #tbl_agg 
	do	for	ii_mcr, tbl_mcr in pairs(tbl_agg[ii_agg])
		do      if tbl_mcr.FileName
			then	Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_agg[ii_agg])
				break
			else	Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_mcr)
			end
		end
		for	ii_mcr = 1, #tbl_agg[ii_agg]
		do      if not tbl_agg[ii_agg][ii_mcr].FileName then
				Xer0X.fnc_mcr_src_tbl_clean(mcr_src, tbl_agg[ii_agg][ii_mcr])
			end
		end
	end
end


Xer0X.fnc_mcr_src_fnc_clean = function(mcr_src, tbl_fnc)
	mcr_src = string.lower(mcr_src)
	local fnc_scr_path
	local rem_cnt = 0
	for	ii_cc = 1, #tbl_fnc - rem_cnt
	do      for	ii_ccf, ccf in pairs(tbl_fnc[ii_cc - rem_cnt])
		do	if type(ccf) == "function" then
				fnc_scr_path = string.match(debug.getinfo(ccf, "S").source, "^@(.+)")
				if mcr_src == string.lower(fnc_scr_path)
				then	table.remove(tbl_fnc, ii_cc - rem_cnt)
					rem_cnt = rem_cnt + 1
					break
				end
			end
		end
	end
end

Xer0X.fnc_mcr_src_all_clean = function(mcr_src, tbl_what)
	if not tbl_what or tbl_what.macro then Xer0X.fnc_mcr_src_tbl_clean(mcr_src, Xer0X.utils.LoadedMacros)	end
	if not tbl_what or tbl_what.pnlmd then Xer0X.fnc_mcr_src_tbl_clean(mcr_src, Xer0X.utils.LoadedPanelModules)end
	if not tbl_what or tbl_what.cmdln then Xer0X.fnc_mcr_src_tbl_clean(mcr_src, Xer0X.utils.AddedPrefixes)	end
	if not tbl_what or tbl_what.mnitm then Xer0X.fnc_mcr_src_tbl_clean(mcr_src, Xer0X.utils.AddedMenuItems)	end
	if not tbl_what or tbl_what.macro then Xer0X.fnc_mcr_src_agg_clean(mcr_src, Xer0X.utils.Areas)          end
	if not tbl_what or tbl_what.event then Xer0X.fnc_mcr_src_agg_clean(mcr_src, Xer0X.utils.Events)		end
	if not tbl_what or tbl_what.cncol then Xer0X.fnc_mcr_src_fnc_clean(mcr_src, Xer0X.utils.ContentColumns) end
end

local tbl_no_fnc_list = { "NoMacro", "NoEvent", "NoMenuItem", "NoCommandLine", "NoPanelModule", "NoContentColumns" }
Xer0X.fnc_macro_one_load = function(
 mcr_info,
	mcr_path, no_clean, no_exec
		)
	mcr_path = far.ConvertPath(mcr_path, 1)
	if not	no_clean
	then	Xer0X.fnc_mcr_src_all_clean(mcr_path)
	end
	local	fnc_file, msg_file = loadfile(mcr_path)
	if not	fnc_file
	then	far.Message(msg_file, "Error loading macro file", nil, "w")
		return
	end
	local tbl_env_mcr_ini = {
		Macro		= function(srctable) Xer0X.utils.AddRegularMacro(	srctable, mcr_path) end,
		Event		= function(srctable) Xer0X.utils.AddEvent(		srctable, mcr_path) end,
		MenuItem	= function(srctable) Xer0X.utils.AddMenuItem(		srctable, mcr_path) end,
	        CommandLine	= function(srctable) Xer0X.utils.AddPrefixes(		srctable, mcr_path) end,
		PanelModule	= function(srctable) Xer0X.utils.AddPanelModule(	srctable, mcr_path) end,
		ContentColumns	= function(srctable) Xer0X.utils.AddContentColumns(	srctable, mcr_path) end,
		LoadRegularFile	= Xer0X.utils_local.LoadRegularFile
	}
	local fnc_dummy = function() end
	for _, ii_no_fnc_name in ipairs(tbl_no_fnc_list)
	do tbl_env_mcr_ini[ii_no_fnc_name] = fnc_dummy
	end
	local mt_G = { __index = _G }
	setmetatable(tbl_env_mcr_ini, mt_G)
	setfenv(fnc_file, tbl_env_mcr_ini)
	if not	no_exec
	then	local	exec_ret, exec_msg = pcall(fnc_file, mcr_path)
		if not	exec_ret
		then	far.Message(exec_msg, "Error loading macro func (introspection-@Xer0X)", nil, "w")
		end
	end
	local tbl_exinfo = {
		fnc_file = fnc_file,
		msg_file = msg_file,
		exec_ret = exec_ret,
		exec_msg = exec_msg,
		mcr_path = mcr_path,
		mcr_info = mcr_info,
		no_clean = no_clean,
		no_exec	 = no_exec ,
	}
	return tbl_env_mcr_ini, tbl_exinfo
end -- fnc_macro_one_load

Xer0X.fnc_macro_dir_load = function(src_dir)
	far.RecursiveSearch(
		src_dir, 
		"*.lua",
		Xer0X.fnc_macro_one_load,
		0
			)
end

-- @@@@@ End of the trick with ad-hoc loading macros  @@@@@ ?

Xer0X.fnc_file_whoami = function(inp_args, from_level, details)
	local	dbg_info = debug.getinfo(from_level or 2, details or "S")
	local	own_file_path, own_file_fold, own_file_name, own_file_extn
	if 	dbg_info.source
	then	if	string.match(dbg_info.short_src, "^%[.*%]$")
		then	own_file_path = dbg_info.short_src
		else	own_file_path = dbg_info.source:match("^@(.+)")
			own_file_fold,
			own_file_name,
			own_file_extn
				= string.match(own_file_path, "(.-)([^/\\]+)([.][^.]+)$")
		end
	end
	local	own_mdl_head, own_mdl_tail, as_module
	local	the_mdl_name = type(inp_args) == "table" and #inp_args > 0 and inp_args[1] or inp_args
	if	the_mdl_name
	and	type(the_mdl_name) == "string"
	then	own_mdl_head = the_mdl_name:match("^(.*)%.")
		own_mdl_tail = the_mdl_name:match("[.](.*)$")
		as_module = (
			the_mdl_name == "load_as_module" or
			the_mdl_name == own_file_name or
			own_mdl_head and
			the_mdl_name:match("%.("..own_file_name..")$") == own_file_name and
			own_file_fold:match("\\("..own_mdl_head..")\\$")==own_mdl_head
				)
	end
	return
		as_module,		
		inp_args,		
		own_file_path,		
		own_file_fold,		
		own_file_name,		
		own_file_extn,		
		dbg_info.short_src,	
		dbg_info.name or
		dbg_info.what,		
		dbg_info.currentline,	
		dbg_info		
end -- fnc_file_whoami

Xer0X.fnc_mcr_src_reload = function(mcr_src, mcr_src_inf_mod, force)
	local dt_inf_new = win.GetFileInfo(mcr_src)
	if not force and dt_inf_new.LastWriteTime == mcr_src_inf_mod then return end
	Xer0X.fnc_load_macro_one(nil, mcr_src)
	Xer0X.fnc_trans_msg("\n"..Xer0X.fnc_norm_script_path(mcr_src).."\n", "Macro reloaded, please rerun the action", "w", "pers")
	return true
end

return {
	fnc_mcr_src_agg_clean	= Xer0X.fnc_mcr_src_agg_clean,
	fnc_mcr_src_all_clean	= Xer0X.fnc_mcr_src_all_clean,
	fnc_mcr_src_fnc_clean	= Xer0X.fnc_mcr_src_fnc_clean,
	fnc_mcr_src_tbl_clean	= Xer0X.fnc_mcr_src_tbl_clean,
	fnc_exec_time_info_read	= Xer0X.fnc_exec_time_info_read,
	fnc_func_name_read	= Xer0X.fnc_func_name_read,
	fnc_macro_dir_load	= Xer0X.fnc_macro_dir_load,
	fnc_macro_one_load	= Xer0X.fnc_macro_one_load,
	fnc_definition_parse	= Xer0X.fnc_definition_parse,
	fnc_menu_dbg_inf_key_weight
				= Xer0X.fnc_menu_dbg_inf_key_weight,
}

-- @@@@@
