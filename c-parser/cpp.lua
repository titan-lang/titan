
local cpp = {}

local typed = require("typed")
local c99 = require("c-parser.c99")
local exps = require("c-parser.exps")

local SEP = package.config:sub(1,1)

local function debug() end
--[[
local inspect = require("inspect")
local function debug(...)
    local args = { ... }
    for i, arg in ipairs(args) do
        if type(arg) == "table" then
            args[i] = inspect(arg)
        end
    end
    print(table.unpack(args))
end

local function is_sequence(xs)
   if type(xs) ~= "table" then
      return false
   end
   local l = #xs
   for k, _ in pairs(xs) do
      if type(k) ~= "number" or k < 1 or k > l or math.floor(k) ~= k then
         return false
      end
   end
   return true
end
--]]

local gcc_default_defines
do
    local default_defines

    local function shallow_copy(t)
        local u = {}
        for k,v in pairs(t) do
            u[k] = v
        end
        return u
    end

    gcc_default_defines = function()
        if default_defines then
            return shallow_copy(default_defines)
        end

        local pd = io.popen("LANG=C gcc -dM -E - < /dev/null")
        if not pd then
            return {}
        end
        local blank_ctx = {
            incdirs = {},
            defines = {},
            ifmode = { true },
            output = {},
            current_dir = {},
        }
        typed.set_type(blank_ctx, "Ctx")
        local ctx = cpp.parse_file("-", pd, blank_ctx)

        ctx.defines["__builtin_va_list"] = { "char", "*" }
        ctx.defines["__extension__"] = {}
        ctx.defines["__attribute__"] = { args = { "arg" }, repl = {} }
        ctx.defines["__restrict__"] = { "restrict" }
        ctx.defines["__restrict"] = { "restrict" }
        ctx.defines["__inline__"] = { "inline" }
        ctx.defines["__inline"] = { "inline" }

        default_defines = ctx.defines
        return shallow_copy(ctx.defines)
    end
end

local function cpp_include_paths()
    local pd = io.popen("LANG=C cpp -v /dev/null -o /dev/null 2>&1")
    if not pd then
        return { quote = {}, system = { "/usr/include"} }
    end
    local res = {
        quote = {},
        system = {},
    }
    local mode = nil
    for line in pd:lines() do
        if line:find([[#include "..." search starts here]], 1, true) then
            mode = "quote"
        elseif line:find([[#include <...> search starts here]], 1, true) then
            mode = "system"
        elseif line:find([[End of search list]], 1, true) then
            mode = nil
        elseif mode then
            table.insert(res[mode], line:sub(2))
        end
    end
    pd:close()
    return res
end

-- TODO default defines: `gcc -dM -E - < /dev/null`

-- Not supported:
-- * character set conversion
-- * trigraphs

local states = {
    any = {
        ['"'] = { next = "dquote" },
        ["'"] = { next = "squote" },
        ["/"] = { silent = true, next = "slash" },
    },
    dquote = {
        ['"'] = { next = "any" },
        ["\\"] = { next = "dquote_backslash" },
    },
    dquote_backslash = {
        single_char = true,
        default = { next = "dquote" },
    },
    squote = {
        ["'"] = { next = "any" },
        ["\\"] = { next = "squote_backslash" },
    },
    squote_backslash = {
        single_char = true,
        default = { next = "squote" },
    },
    slash = {
        single_char = true,
        ["/"] = { add = " ", silent = true, next = "line_comment" },
        ["*"] = { add = " ", silent = true, next = "block_comment" },
        default = { add = "/", next = "any" },
    },
    line_comment = {
        silent = true,
    },
    block_comment = {
        silent = true,
        ["*"] = { silent = true, next = "try_end_block_comment" },
        continue_line = "block_comment",
    },
    try_end_block_comment = {
        single_char = true,
        silent = true,
        ["/"] = { silent = true, next = "any" },
        ["*"] = { silent = true, next = "try_end_block_comment" },
        default = { silent = true, next = "block_comment" },
        continue_line = "block_comment",
    },
}

for _, rules in pairs(states) do
    local out = "["
    for k, _ in pairs(rules) do
        if #k == 1 then
            out = out .. k
        end
    end
    out = out .. "]"
    rules.pattern = out ~= "[]" and out
end

local function add(buf, txt)
    if not buf then
        buf = {}
    end
    table.insert(buf, txt)
    return buf
end

cpp.initial_processing = typed("FILE* -> LineList", function(fd)
    local backslash_buf
    local buf
    local state = "any"
    local output = {}
    local linenr = 0
    for line in fd:lines() do
        linenr = linenr + 1
        local len = #line
        if line:find("\\", len, true) then
            -- If backslash-terminated, buffer it
            backslash_buf = add(backslash_buf, line:sub(1, len - 1))
        else
            -- Merge backslash-terminated line
            if backslash_buf then
                table.insert(backslash_buf, line)
                line = table.concat(backslash_buf)
            end
            backslash_buf = nil

            len = #line
            local i = 1
            local out = ""
            -- Go through the line
            while i <= len do
                -- Current state in the state machine
                local st = states[state]

                -- Look for next character matching a state transition
                local n = nil
                if st.pattern then
                    if st.single_char then
                        if line:sub(i,i):find(st.pattern) then
                            n = i
                        end
                    else
                        n = line:find(st.pattern, i)
                    end
                end

                local transition, ch
                if n then
                    ch = line:sub(n, n)
                    transition = st[ch]
                else
                    n = i
                    ch = line:sub(n, n)
                    transition = st.default
                end

                if not transition then
                    -- output the rest of the string if we should
                    if not st.silent then
                        out = i == 1 and line or line:sub(i)
                    end
                    break
                end

                -- output everything up to the transition if we should
                if n > i and not st.silent then
                    buf = add(buf, line:sub(i, n - 1))
                end

                -- Some transitions output an explicit character
                if transition.add then
                    buf = add(buf, transition.add)
                end

                if not transition.silent then
                    buf = add(buf, ch)
                end

                -- and move to the next state
                state = transition.next
                i = n + 1
            end

            -- If we ended in a non-line-terminating state
            if states[state].continue_line then
                -- buffer the output and keep going
                buf = add(buf, out)
                state = states[state].continue_line
            else
                -- otherwise, flush the buffer
                if buf then
                    table.insert(buf, out)
                    out = table.concat(buf)
                    buf = nil
                end
                -- output the string and reset the state.
                table.insert(output, { nr = linenr, line = out})
                state = "any"
            end
        end
    end
    fd:close()
    typed.set_type(output, "LineList")
    return output
end)

cpp.tokenize = typed("string -> table", function(line)
    return c99.match_preprocessing_grammar(line)
end)

local function find_file(ctx, filename, mode, is_next)
    local paths = {}
    local current_dir = ctx.current_dir[#ctx.current_dir]
    if mode == "quote" or is_next then
        if not is_next then
            table.insert(paths, current_dir)
        end
        for _, incdir in ipairs(ctx.incdirs.quote or {}) do
            table.insert(paths, incdir)
        end
    end
    if mode == "system" or is_next then
        for _, incdir in ipairs(ctx.incdirs.system or {}) do
            table.insert(paths, incdir)
        end
    end
    if is_next then
        while paths[1] and paths[1] ~= current_dir do
            table.remove(paths, 1)
        end
        table.remove(paths, 1)
    end
    for _, path in ipairs(paths) do
        local pathname = path..SEP..filename
        local fd, err = io.open(pathname, "r")
        if fd then
            return pathname, fd
        end
    end
    return nil, nil, "file not found"
end

local consume_parentheses = typed("{string}, number, LineList, number -> {{string}}, number", function(tokens, start, linelist, curline)
    local args = {}
    local i = start + 1
    local arg = {}
    local stack = 0
    while true do
        local token = tokens[i]
        if token == nil then
            repeat
                curline = curline + 1
                if not linelist[curline] then
                    error("unterminated function-like macro")
                end
                local nextline = linelist[curline].tk
                linelist[curline].tk = {}
                table.move(nextline, 1, #nextline, i, tokens)
                token = tokens[i]
            until token
        end
        if token == "(" then
            stack = stack + 1
            table.insert(arg, token)
        elseif token == ")" then
            if stack == 0 then
                if #arg > 0 then
                    table.insert(args, arg)
                end
                break
            end
            stack = stack - 1
            table.insert(arg, token)
        elseif token == "," then
            if stack == 0 then
                table.insert(args, arg)
                arg = {}
            else
                table.insert(arg, token)
            end
        else
            table.insert(arg, token)
        end
        i = i + 1
    end
    return args, i
end)

local function array_copy(t)
    local t2 = {}
    for i,v in ipairs(t) do
        t2[i] = v
    end
    return t2
end

local function table_remove(list, pos, n)
    table.move(list, pos + n, #list + n, pos)
end

local function table_replace_n_with(list, at, n, values)
    local old = #list
    debug("rplc?", list, "AT", at, "N", n, "VALUES", values)
    --assert(is_sequence(list))
    local nvalues = #values
    local nils = n >= nvalues and (n - nvalues + 1) or 0
    if n ~= nvalues then
        table.move(list, at + n, #list + nils, at + nvalues)
    end
    debug("....", list)
    table.move(values, 1, nvalues, at, list)
    --assert(is_sequence(list))
    debug("rplc!", list)
    assert(#list == old - n + #values)
end

local stringify = typed("{string} -> string", function(tokens)
    return '"'..table.concat(tokens, " "):gsub("\"", "\\")..'"'
end)

local macro_expand

local LoopChecker
LoopChecker = {
    new = function(parent, offset)
        local self = {}
        self.tokens = parent and parent.tokens or {}
        self.offset = offset and (offset + parent.offset) or 0
        return typed.table("LoopChecker", setmetatable(self, { __index = LoopChecker }))
    end,
    mark = typed("LoopChecker, string, number -> ()", function(self, token, n)
        n = n + self.offset
        self.tokens[token] = math.max(self.tokens[token] or 0, n)
    end),
    shift = typed("LoopChecker, number -> ()", function(self, n)
        for token, v in pairs(self.tokens) do
            self.tokens[token] = v + n
        end
    end),
    valid = typed("LoopChecker, string, number -> boolean", function(self, token, n)
        n = n + self.offset
        return self.tokens[token] == nil or self.tokens[token] < n
    end),
}

local replace_args = typed("Ctx, {string}, table, LineList, number, LoopChecker? -> ()", function(ctx, tokens, args, linelist, curline, loopchecker)
    local i = 1
    local hash_next = false
    local join_next = false
    while true do
        local token = tokens[i]
        if not token then
            break
        end
        if token == "#" then
            hash_next = true
            table.remove(tokens, i)
        elseif token == "##" then
            join_next = true
            table.remove(tokens, i)
        elseif args[token] then
            macro_expand(ctx, args[token], linelist, curline, false, loopchecker)
            if hash_next then
                tokens[i] = stringify(args[token])
                hash_next = false
            elseif join_next then
                tokens[i - 1] = tokens[i - 1] .. table.concat(args[token], " ")
                table.remove(tokens, i)
                join_next = false
            else
                table_replace_n_with(tokens, i, 1, args[token])
                debug("rplcd", token, args[token], tokens)
                i = i + #args[token]
            end
        elseif join_next then
            tokens[i - 1] = tokens[i - 1] .. tokens[i]
            table.remove(tokens, i)
            join_next = false
        else
            hash_next = false
            join_next = false
            i = i + 1
        end
    end
end)

macro_expand = typed("Ctx, {string}, LineList, number, boolean, LoopChecker? -> ()", function(ctx, tokens, linelist, curline, expr_mode, loopchecker)
    local i = 1
    loopchecker = loopchecker or LoopChecker.new()
    while true do
        ::continue::
        debug("mexp", i, tokens)
        local token = tokens[i]
        if not token then
            break
        end
        if expr_mode then
            if token == "defined" then
                if tokens[i + 1] == "(" then
                    i = i + 2
                end
                i = i + 2
                goto continue
            end
        end
        local define = ctx.defines[token]
        if define and loopchecker:valid(token, i) then
            debug("define" ,token, define)
            local repl = define.repl
            if define.args then
                if tokens[i + 1] == "(" then
                    local args, j = consume_parentheses(tokens, i + 1, linelist, curline)
                    debug("args:", #args, args)
                    local named_args = {}
                    for i = 1, #define.args do
                        named_args[define.args[i]] = args[i] or {}
                    end
                    local expansion = array_copy(repl)
                    replace_args(ctx, expansion, named_args, linelist, curline, nil)
                    local nexpansion = #expansion
                    local n = j - i + 1
                    if nexpansion == 0 then
                        table_remove(tokens, i, n)
                    else
                        table_replace_n_with(tokens, i, n, expansion)
                    end
                    loopchecker:shift(nexpansion - n)
                    --loopchecker:mark(token, i + nexpansion - 1)
                else
                    i = i + 1
                end
            else
                local ndefine = #define
                if ndefine == 0 then
                    table.remove(tokens, i)
                    loopchecker:shift(-1)
                elseif ndefine == 1 then
                    tokens[i] = define[1]
                    loopchecker:mark(token, i)
                else
                    table_replace_n_with(tokens, i, 1, define)
                    loopchecker:mark(token, i + ndefine - 1)
                end
            end
        else
            i = i + 1
        end
    end
end)

local memoized_linelists = {}

cpp.parse_file = typed("string, FILE*?, Ctx? -> Ctx?, string?", function(filename, fd, ctx)
    if not ctx then
        ctx = {
            incdirs = cpp_include_paths(),
            defines = gcc_default_defines(),
            ifmode = { true },
            output = {},
            current_dir = {}
        }
        typed.set_type(ctx, "Ctx")
        -- if not absolute path
        if not filename:match("^/") then
            local found_name, found_fd = find_file(ctx, filename, "system")
            if found_fd then
                filename, fd = found_name, found_fd
            end
        end
    end

    local current_dir = filename:gsub("/[^/]*$", "")
    if current_dir == filename then
        current_dir = "."
        local found_name, found_fd = find_file(ctx, filename, "system")
        if found_fd then
            filename, fd = found_name, found_fd
        end
    end
    table.insert(ctx.current_dir, current_dir)

    local linelist, err

    linelist = memoized_linelists[filename]
    if not linelist then
        if not fd then
            fd, err = io.open(filename, "rb")
            if not fd then
                return nil, err
            end
        end
        linelist = cpp.initial_processing(fd)
        for _, lineitem in ipairs(linelist) do
            lineitem.tk = cpp.tokenize(lineitem.line)
        end
        memoized_linelists[filename] = linelist
    end

    local ifmode = ctx.ifmode
    for curline, lineitem in ipairs(linelist) do
        local line = lineitem.line
        local tk = lineitem.tk
        debug("line", filename, curline, ifmode[#ifmode], #ifmode, line)

        if #ifmode == 1 and (tk.directive == "elif" or tk.directive == "else" or tk.directive == "endif") then
            return nil, "unexpected directive " .. tk.directive
        end

        if tk.exp then
            macro_expand(ctx, tk.exp, linelist, curline, true, nil)
        end

        if ifmode[#ifmode] == true then
            if tk.directive then
                debug("drctv", tk)
            end
            if tk.directive == "define" then
                local k = tk.id
                local v = tk.args and tk or tk.repl
                ctx.defines[k] = v
            elseif tk.directive == "undef" then
                ctx.defines[tk.id] = nil
            elseif tk.directive == "ifdef" then
                table.insert(ifmode, (ctx.defines[tk.id] ~= nil))
            elseif tk.directive == "ifndef" then
                table.insert(ifmode, (ctx.defines[tk.id] == nil))
            elseif tk.directive == "if" then
                table.insert(ifmode, exps.run_expression(ctx, tk.exp))
            elseif tk.directive == "elif" then
                ifmode[#ifmode] = "skip"
            elseif tk.directive == "else" then
                ifmode[#ifmode] = not ifmode[#ifmode]
            elseif tk.directive == "endif" then
                table.remove(ifmode, #ifmode)
            elseif tk.directive == "error" or tk.directive == "pragma" then
                -- ignore
            elseif tk.directive == "include" or tk.directive == "include_next" then
                local name = tk.exp[1]
                local mode = tk.exp.mode
                local is_next = (tk.directive == "include_next")
                local inc_filename, inc_fd, err = find_file(ctx, name, mode, is_next)
                if not inc_filename then
                    return nil, name..":"..err
                end
                cpp.parse_file(inc_filename, inc_fd, ctx)
            else
                macro_expand(ctx, tk, linelist, curline, false, nil)
                table.insert(ctx.output, table.concat(tk, " "))
            end
        elseif ifmode[#ifmode] == false then
            if tk.directive == "ifdef"
            or tk.directive == "ifndef"
            or tk.directive == "if" then
                table.insert(ifmode, "skip")
            elseif tk.directive == "else" then
                ifmode[#ifmode] = not ifmode[#ifmode]
            elseif tk.directive == "elif" then
                ifmode[#ifmode] = exps.run_expression(ctx, tk.exp)
            elseif tk.directive == "endif" then
                table.remove(ifmode, #ifmode)
            end
        elseif ifmode[#ifmode] == "skip" then
            if tk.directive == "ifdef"
            or tk.directive == "ifndef"
            or tk.directive == "if" then
                table.insert(ifmode, "skip")
            elseif tk.directive == "else"
                or tk.directive == "elif" then
                -- do nothing
            elseif tk.directive == "endif" then
                table.remove(ifmode, #ifmode)
            end
        end
    end

    table.remove(ctx.current_dir)

    return ctx, nil
end)

cpp.expand_macro = typed("string, table -> string", function(macro, define_set)
    local ctx = typed.table("Ctx", setmetatable({
        defines = define_set,
    }, { __index = error, __newindex = error }))
    local tokens = { macro }
    local linelist = typed.table("LineList", { { nr = 1, line = macro } })
    macro_expand(ctx, tokens, linelist, 1, false, nil)
    return table.concat(tokens, " ")
end)

return cpp
