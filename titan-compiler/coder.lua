local types = require "titan-compiler.types"
local util  = require "titan-compiler.util"

local coder = {}

local codeexp, codestat, codeforeignexp

local render = util.render

--
-- Functions for C literals
--

-- Technically, we only need to escape the quote and backslash
-- But quoting some extra things helps readability...
local some_c_escape_sequences = {
    ["\\"] = "\\\\",
    ["\""] = "\\\"",
    ["\a"] = "\\a",
    ["\b"] = "\\b",
    ["\f"] = "\\f",
    ["\n"] = "\\n",
    ["\r"] = "\\r",
    ["\t"] = "\\t",
    ["\v"] = "\\v",
}

local function c_string_literal(s)
    return '"' .. (s:gsub('.', some_c_escape_sequences)) .. '"'
end

local function c_integer_literal(n)
    return string.format("%i", n)
end

local function c_float_literal(n)
    return string.format("%f", n)
end


-- Is this expression a numeric literal?
-- If yes, return that number. If not, returns nil.
--
-- This limited form of constant-folding is enough to optimize things like for
-- loops, since most of the time the loop step is a numeric literal.
-- Note to self: A constant-folding optimization pass would obsolete this
local function node2literal(node)
    local tag = node._tag
    if tag == "Ast.ExpInteger" or tag == "Ast.ExpFloat" then
        return tonumber(node.value)
    elseif tag == "Ast.ExpUnop" and node.op == "-" then
        local lexp = node2literal(node.exp)
        return lexp and -lexp
    else
        return nil
    end
end

local function getslot(typ --[[:table]], dst --[[:string?]], src --[[:string]])
    dst = dst and dst .. " =" or ""
    local tmpl
    if typ._tag == "Type.Integer" then tmpl = "$DST ivalue($SRC)"
    elseif typ._tag == "Type.Float" then tmpl = "$DST fltvalue($SRC)"
    elseif typ._tag == "Type.Boolean" then tmpl = "$DST bvalue($SRC)"
    elseif typ._tag == "Type.Nil" then tmpl = "$DST 0"
    elseif typ._tag == "Type.String" then tmpl = "$DST tsvalue($SRC)"
    elseif typ._tag == "Type.Array" then tmpl = "$DST hvalue($SRC)"
    elseif typ._tag == "Type.Map" then tmpl = "$DST hvalue($SRC)"
    elseif typ._tag == "Type.Value" then tmpl = "$DST *($SRC)"
    elseif typ._tag == "Type.Nominal" then tmpl = "$DST clCvalue($SRC)"
    else error("invalid type " .. types.tostring(typ)) end
    return render(tmpl, { DST = dst, SRC = src })
end

local function mangle_qn(qualname)
    return qualname:gsub("[%-.]", "_")
end

local function tagname(fqtn)
    return mangle_qn(fqtn) .. "_typetag"
end

local function fname_prefix(typ)
    return mangle_qn(typ.fqtn) .. "_" .. typ.name
end

local function static_name(typ)
    return fname_prefix(typ) .. "_titanstatic"
end

local function static_luaname(typ)
    return fname_prefix(typ) .. "_luastatic"
end

local function method_name(typ)
    return fname_prefix(typ) .. "_titanmethod"
end

local function method_luaname(typ)
    return fname_prefix(typ) .. "_luamethod"
end

local function modprefix(modname)
    return mangle_qn(modname) .. "_"
end

local function func_name(modname, name)
    return modprefix(modname) .. name .. "_titan"
end

local function func_luaname(modname, name)
    return modprefix(modname) .. name .. "_lua"
end

local function var_name(modname, name)
    return modprefix(modname) .. name .. "_titanvar"
end

-- get name of the variable holding the tag for this type
local function type2tagname(ctx, typ --[[:table]])
    local mod, name = typ.fqtn:match("^(.*)%.(%a+)$")
    local mangled = tagname(typ.fqtn)
    if mod == ctx.module then
        -- local tag
        return "((ptrdiff_t)&" .. mangled .. ")"
    else
        -- imported tag
        ctx.tags[typ.fqtn] = { module = mod, type = name, mangled = mangled }
        return mangled
    end
end

local function metaname(fqtn)
    return mangle_qn(fqtn) .. "_typemt"
end

-- get name of the variable holding the metatable for this type
local function type2metatable(ctx, typ --[[:table]])
    local mod, name = typ.fqtn:match("^(.*)%.(%a+)$")
    local mangled = metaname(typ.fqtn)
    if mod ~= ctx.module then
        ctx.metatables[typ.fqtn] = { module = mod, type = name, mangled = mangled }
    end
    return mangled
end

local function checkandget(ctx, typ --[[:table]], cvar --[[:string]], exp --[[:string]], loc --[[:table]])
    local tag
    if typ._tag == "Type.Integer" then
        return render([[
            if (TITAN_LIKELY(ttisinteger($EXP))) {
                $VAR = ivalue($EXP);
            } else if (ttisfloat($EXP)) {
                float _v = fltvalue($EXP);
                float _flt = l_floor(_v);
                if (TITAN_UNLIKELY(_v != _flt)) {
                    luaL_error(L, "%s:%d:%d: type error, number '%f' has no integer representation", $FILE, $LINE, $COL, _v);
                } else {
                    lua_numbertointeger(_flt, &$VAR);
                }
            } else {
                luaL_error(L, "%s:%d:%d: type error, expected integer but found %s", $FILE, $LINE, $COL, lua_typename(L, ttnov($EXP)));
            }
        ]], {
            EXP = exp,
            VAR = cvar,
            FILE = c_string_literal(loc.filename),
            LINE = c_integer_literal(loc.line),
            COL = c_integer_literal(loc.col),
        })
    elseif typ._tag == "Type.Float" then
        return render([[
            if (TITAN_LIKELY(ttisfloat($EXP))) {
                $VAR = fltvalue($EXP);
            } else if (ttisinteger($EXP)) {
                $VAR = (lua_Number)ivalue($EXP);
            } else {
                luaL_error(L, "%s:%d:%d: type error, expected float but found %s", $FILE, $LINE, $COL, lua_typename(L, ttnov($EXP)));
            }
        ]], {
            EXP = exp,
            VAR = cvar,
            FILE = c_string_literal(loc.filename),
            LINE = c_integer_literal(loc.line),
            COL = c_integer_literal(loc.col),
        })
    elseif typ._tag == "Type.Boolean" then
        return render([[
            if (l_isfalse($EXP)) {
                $VAR = 0;
            } else {
                $VAR = 1;
            }
        ]], {
            EXP = exp,
            VAR = cvar
        })
    elseif typ._tag == "Type.Nil" then tag = "nil"
    elseif typ._tag == "Type.String" then tag = "string"
    elseif typ._tag == "Type.Array" then tag = "table"
    elseif typ._tag == "Type.Map" then tag = "table"
    elseif typ._tag == "Type.Value" then
        return render([[
            setobj2t(L, &$VAR, $EXP);
        ]], {
            EXP = exp,
            VAR = cvar
        })
    elseif typ._tag == "Type.Nominal" then
        return render([[
            if (TITAN_LIKELY(ttisfulluserdata($EXP))) {
              Udata* _ud = uvalue($EXP);
              ptrdiff_t _tag = *((ptrdiff_t*)(getudatamem(_ud)));
              if (TITAN_LIKELY($TAG == _tag)) {
                $VAR = gco2ccl(_ud->user_.gc);
              } else {
                lua_pushlightuserdata(L, (void*)_tag);
                if(lua_rawget(L, LUA_REGISTRYINDEX) == LUA_TNIL) {
                    setuvalue(L, L->top, _ud);
                    luaL_error(L, "%s:%d:%d: type error, expected %s but found %s", $FILE, $LINE, $COL, "$NAME", luaL_tolstring(L, -1, NULL));
                } else {
                    luaL_error(L, "%s:%d:%d: type error, expected %s but found %s", $FILE, $LINE, $COL, "$NAME", lua_tostring(L, -1));
                }
              }
            } else {
              luaL_error(L, "%s:%d:%d: type error, expected %s but found %s", $FILE, $LINE, $COL, "$NAME", lua_typename(L, ttnov($EXP)));
            }
        ]], {
            TAG = type2tagname(ctx, typ),
            FILE = c_string_literal(loc.filename),
            LINE = c_integer_literal(loc.line),
            COL = c_integer_literal(loc.col),
            EXP = exp,
            VAR = cvar,
            NAME = types.tostring(typ)
        })
    else
        error("invalid type " .. types.tostring(typ))
    end
    return render([[
        if (TITAN_LIKELY($PREDICATE($EXP))) {
            $GETSLOT;
        } else {
            luaL_error(L, "%s:%d:%d: type error, expected %s but found %s", $FILE, $LINE, $COL, $TAG, lua_typename(L, ttnov($EXP)));
        }
    ]], {
        EXP = exp,
        TAG = c_string_literal(tag),
        PREDICATE = 'ttis'..tag,
        GETSLOT = getslot(typ, cvar, exp),
        FILE = c_string_literal(loc.filename),
        LINE = c_integer_literal(loc.line),
        COL = c_integer_literal(loc.col),
})
end

local function checkandset(ctx, typ --[[:table]], dst --[[:string]], src --[[:string]], loc --[[:table]])
    local tag
    if typ._tag == "Type.Integer" then tag = "integer"
    elseif typ._tag == "Type.Float" then
        return render([[
            if (TITAN_LIKELY(ttisfloat($SRC))) {
                setobj2t(L, $DST, $SRC);
            } else if (ttisinteger($SRC)) {
                setfltvalue($DST, ((lua_Number)ivalue($SRC)));
            } else {
                luaL_error(L, "%s:%d:%d: type error, expected float but found %s", $FILE, $LINE, $COL, lua_typename(L, ttnov($SRC)));
            }
        ]], {
            SRC = src,
            DST = dst,
            FILE = c_string_literal(loc.filename),
            LINE = c_integer_literal(loc.line),
            COL = c_integer_literal(loc.col),
        })
    elseif typ._tag == "Type.Boolean" then tag = "boolean"
    elseif typ._tag == "Type.Nil" then tag = "nil"
    elseif typ._tag == "Type.String" then tag = "string"
    elseif typ._tag == "Type.Array" then tag = "table"
    elseif typ._tag == "Type.Map" then tag = "table"
    elseif typ._tag == "Type.Value" then
        return render([[
            setobj2t(L, $DST, $SRC);
        ]], {
            SRC = src,
            DST = dst,
        })
    elseif typ._tag == "Type.Nominal" then
        return render([[
            {
              CClosure *_cl = NULL;
              $CHECKANDGET
              setclCvalue(L, $DST, _cl);
            }
        ]], {
            CHECKANDGET = checkandget(ctx, typ, "_cl", src, loc),
            DST = dst
        })
    else
        error("invalid type " .. types.tostring(typ))
    end
    return render([[
        if (TITAN_LIKELY($PREDICATE($SRC))) {
            setobj2t(L, $DST, $SRC);
        } else {
            luaL_error(L, "%s:%d:%d: type error, expected %s but found %s", $FILE, $LINE, $COL, $TAG, lua_typename(L, ttnov($SRC)));
        }
    ]], {
        TAG = c_string_literal(tag),
        PREDICATE = 'ttis'..tag,
        SRC = src,
        DST = dst,
        FILE = c_string_literal(loc.filename),
        LINE = c_integer_literal(loc.line),
        COL = c_integer_literal(loc.col),
})
end

local function setslot(typ --[[:table]], dst --[[:string]], src --[[:string]])
    local tmpl
    if typ._tag == "Type.Integer" then tmpl = "setivalue($DST, $SRC);"
    elseif typ._tag == "Type.Float" then tmpl = "setfltvalue($DST, $SRC);"
    elseif typ._tag == "Type.Boolean" then tmpl = "setbvalue($DST, $SRC);"
    elseif typ._tag == "Type.Nil" then tmpl = "setnilvalue($DST); ((void)$SRC);"
    elseif typ._tag == "Type.String" then tmpl = "setsvalue(L, $DST, $SRC);"
    elseif typ._tag == "Type.Array" then tmpl = "sethvalue(L, $DST, $SRC);"
    elseif typ._tag == "Type.Map" then tmpl = "sethvalue(L, $DST, $SRC);"
    elseif typ._tag == "Type.Value" then tmpl = "setobj2t(L, $DST, &$SRC);"
    elseif typ._tag == "Type.Nominal" then tmpl = "setclCvalue(L, $DST, $SRC);"
    else
        error("invalid type " .. types.tostring(typ))
    end
    local code = render(tmpl, { DST = dst, SRC = src })
    return code
end

local function setwrapped(typ --[[:table]], dst --[[:string]], src --[[:string]])
    if typ._tag == "Type.Nominal" then
        return render([[
            setobj2t(L, $DST, &(($SRC)->upvalue[0]));
        ]], {
            DST = dst,
            SRC = src
        })
    else
        return setslot(typ, dst, src)
    end
end

local function foreignctype(typ --[[:table]])
    if typ._tag == "Type.Pointer" then
        if typ.type._tag == "Type.Nil" then
            return "void*"
        end
        if typ.type._tag == "Type.Typedef" then
            return typ.type.name .. "*"
        end
        return foreignctype(typ.type) .. "*"
    elseif typ._tag == "Type.String" then
        return "char*"
    end
    error("FIXME unknown foreign c type: "..types.tostring(typ))
end

local function foreigncast(exp, typ --[[:table]])
    return render([[(($CASTTYPE)($CEXP))]], {
        CASTTYPE = foreignctype(typ),
        CEXP = exp,
    })
end

local function ctype(typ --[[:table]])
    if typ._tag == "Type.Integer" then return "lua_Integer"
    elseif typ._tag == "Type.Float" then return "lua_Number"
    elseif typ._tag == "Type.Boolean" then return "int"
    elseif typ._tag == "Type.Nil" then return "int"
    elseif typ._tag == "Type.String" then return "TString*"
    elseif typ._tag == "Type.Array" then return "Table*"
    elseif typ._tag == "Type.Map" then return "Table*"
    elseif typ._tag == "Type.Value" then return "TValue"
    elseif typ._tag == "Type.Nominal" then return "CClosure*"
    elseif typ._tag == "Type.Pointer" then return foreignctype(typ)
    else error("invalid type " .. types.tostring(typ))
    end
end

local function initval(typ --[[:table]])
    if typ._tag == "Type.Value" then return "{ {0}, 0 }"
    else return "0" end
end


local function function_sig(fname, ftype, is_pointer)
    local params = { "lua_State *L" }
    if ftype._tag == "Type.Method" then
        table.insert(params, "CClosure*")
    end
    for i, ptype in ipairs(ftype.params) do
        table.insert(params, ctype(ptype))
    end
    for i = 2, #ftype.rettypes do
        table.insert(params, ctype(ftype.rettypes[i]) .. "*")
    end
    local rettype = ftype.rettypes[1]
    local template = is_pointer
                     and "$RETTYPE (*$FNAME)($PARAMS)"
                     or  "$RETTYPE $FNAME($PARAMS)"
    return render(template, {
        RETTYPE = ctype(rettype),
        FNAME = fname,
        PARAMS = table.concat(params, ", ")
    })
end

-- creates a new code generation context for a function
local function newcontext(tlcontext)
    return {
        tmp = 1,    -- next temporary index (for generating temporary names)
        nslots = 0, -- number of slots needed by function
        allocations = 0, -- number of allocations
        depth = 0,  -- current stack depth
        dstack = {}, -- stack of stack depths
        prefix = tlcontext.prefix, -- prefix for module member functions and variables
        names = {}, -- map of names to autoincrement suffixes for disambiguation,
        tags = tlcontext.tags or {}, -- set of external record types that we need tags of
        metatables = tlcontext.metatables, -- set of external metatables that we need
        functions = tlcontext.functions, -- set of external functions and methods that we call
        variables = tlcontext.variables, -- set of external variables that we use
        module = tlcontext.module, -- module name
    }
end

local function newslot(ctx --[[:table]], name --[[:string]])
    local sdepth = ctx.depth
    ctx.depth = ctx.depth + 1
    ctx.allocations = ctx.allocations + 1
    if ctx.depth > ctx.nslots then ctx.nslots = ctx.depth end
    return render([[
        TValue *$NAME = _base + $SDEPTH;
    ]], {
        NAME = name,
        SDEPTH = c_integer_literal(sdepth),
    })
end

local function newtmp(ctx --[[:table]], typ --[[:table]], isgc --[[:boolean]])
    local tmp = ctx.tmp
    ctx.tmp = ctx.tmp + 1
    local tmpname = "_tmp_" .. tmp
    if isgc then
        local slotname = "_tmp_" .. tmp .. "_slot"
        return render([[
            $NEWSLOT
            $TYPE $TMPNAME = $INIT;
        ]], {
            TYPE = ctype(typ),
            NEWSLOT = newslot(ctx, slotname),
            TMPNAME = tmpname,
            INIT = initval(typ)
        }), tmpname, slotname
    else
        return render([[
            $TYPE $TMPNAME = $INIT;
        ]], {
            TYPE = ctype(typ),
            TMPNAME = tmpname,
            INIT = initval(typ)
        }), tmpname
    end
end

local function newforeigntmp(ctx --[[:table]], typ --[[:table]])
    local tmp = ctx.tmp
    ctx.tmp = ctx.tmp + 1
    local tmpname = "_tmp_" .. tmp
    local ftype
    if typ._tag == "Type.String" then
        ftype = "char*"
    elseif typ._tag == "Type.Pointer" then
        ftype = foreignctype(typ)
    else
        error("don't know how to convert foreign type "..types.tostring(typ))
    end
    return render([[
        $TYPE $TMPNAME;
    ]], {
        TYPE = ftype,
        TMPNAME = tmpname,
    }), tmpname
end

local function pushd(ctx)
    table.insert(ctx.dstack, ctx.depth)
end

local function popd(ctx)
    ctx.depth = table.remove(ctx.dstack)
end

-- All the code generation functions for STATEMENTS take
-- the function context and the AST node and return the
-- generated C code for the statement, as a string

local function codeblock(ctx, node)
    local stats = {}
    pushd(ctx)
    for _, stat in ipairs(node.stats) do
        table.insert(stats, codestat(ctx, stat))
    end
    popd(ctx)
    return " {\n " .. table.concat(stats, "\n ") .. "\n }"
end

local function codewhile(ctx, node)
    pushd(ctx)
    local nallocs = ctx.allocations
    local cstats, cexp = codeexp(ctx, node.condition, true)
    local cblk = codestat(ctx, node.block)
    nallocs = ctx.allocations - nallocs
    popd(ctx)
    local tmpl
    if cstats == "" then
        tmpl = [[
            while($CEXP) {
                $CBLK
                $CHECKGC
            }
        ]]
    else
        tmpl = [[
            while(1) {
                $CSTATS
                if(!($CEXP)) {
                    break;
                }
                $CBLK
                $CHECKGC
            }
        ]]
    end
    return render(tmpl, {
        CSTATS = cstats,
        CEXP = cexp,
        CBLK = cblk,
        CHECKGC = nallocs > 0 and "luaC_checkGC(L);" or ""
    })
end

local function coderepeat(ctx, node)
    pushd(ctx)
    local nallocs = ctx.allocations
    local cstats, cexp = codeexp(ctx, node.condition, true)
    local cblk = codestat(ctx, node.block)
    nallocs = ctx.allocations - nallocs
    popd(ctx)
    return render([[
        while(1) {
            $CBLK
            $CSTATS
            if($CEXP) {
                break;
            }
            $CHECKGC
        }
    ]], {
        CBLK = cblk,
        CSTATS = cstats,
        CEXP = cexp,
        CHECKGC = nallocs > 0 and "luaC_checkGC(L);" or ""
    })
end

local function codeif(ctx, node, idx)
    idx = idx or 1
    local cstats, cexp, cthn, cels
    if idx == #node.thens then -- last condition
        cstats, cexp = codeexp(ctx, node.thens[idx].condition, true)
        cthn = codestat(ctx, node.thens[idx].block)
        cels = node.elsestat and "else " .. codestat(ctx, node.elsestat):match("^[ \t]*(.*)") or ""
    else
        cstats, cexp = codeexp(ctx, node.thens[idx].condition, true)
        cthn = codestat(ctx, node.thens[idx].block)
        cels = "else " .. codeif(ctx, node, idx + 1):match("^[ \t]*(.*)")
    end
    return render([[
        {
            $CSTATS
            if($CEXP) {
                $CTHN
            } $CELS
        }
    ]], {
        CSTATS = cstats,
        CEXP = cexp,
        CTHN = cthn,
        CELS = cels
    })
end

local function codefor(ctx, node)
    pushd(ctx)
    node.decl._cvar = "_local_" .. node.decl.name
    local cdecl = ctype(node.decl._type) .. " " .. node.decl._cvar
    local csstats, csexp = codeexp(ctx, node.start)
    local cfstats, cfexp = codeexp(ctx, node.finish)
    local cinc = ""
    local cvtyp
    if node.decl._type._tag == "Type.Integer" then
        cvtyp = "lua_Integer"
    else
        cvtyp = "lua_Number"
    end
    local cstart = render([[
        $CSSTATS
        $CVTYP _forstart = $CSEXP;
    ]], {
        CSSTATS = csstats,
        CSEXP = csexp,
        CVTYP = cvtyp,
    })
    local cfinish = render([[
        $CFSTATS
        $CVTYP _forlimit = $CFEXP;
    ]], {
        CFSTATS = cfstats,
        CFEXP = cfexp,
        CVTYP = cvtyp,
    })
    local cstep, ccmp
    local subs = {
        CVAR = node.decl._cvar,
    }
    local ilit = node2literal(node.inc)
    if ilit then
        if ilit > 0 then
            local tmpl
            if node.decl._type._tag == "Type.Integer" then
                subs.ILIT = c_integer_literal(ilit)
                tmpl = "$CVAR = l_castU2S(l_castS2U($CVAR) + $ILIT)"
            else
                subs.ILIT = c_float_literal(ilit)
                tmpl = "$CVAR += $ILIT"
            end
            cstep = render(tmpl, subs)
            ccmp = render("$CVAR <= _forlimit", subs)
        else
            if node.decl._type._tag == "Type.Integer" then
                subs.NEGILIT = c_integer_literal(-ilit)
                cstep = render("$CVAR = l_castU2S(l_castS2U($CVAR) - $NEGILIT)", subs)
            else
                subs.NEGILIT = c_float_literal(-ilit)
                cstep = render("$CVAR -= $NEGILIT", subs)
            end
            ccmp = render("_forlimit <= $CVAR", subs)
        end
    else
        local cistats, ciexp = codeexp(ctx, node.inc)
        cinc = render([[
            $CISTATS
            $CVTYP _forstep = $CIEXP;
        ]], {
            CISTATS = cistats,
            CIEXP = ciexp,
            CVTYP = cvtyp,
        })
        local tmpl
        if node.decl._type._tag == "Type.Integer" then
            tmpl = "$CVAR = l_castU2S(l_castS2U($CVAR) + l_castS2U(_forstep))"
        else
            tmpl = "$CVAR += _forstep"
        end
        cstep = render(tmpl, subs)
        ccmp = render("0 < _forstep ? ($CVAR <= _forlimit) : (_forlimit <= $CVAR)", subs)
    end
    local nallocs = ctx.allocations
    local cblock = codestat(ctx, node.block)
    nallocs = ctx.allocations - nallocs
    popd(ctx)
    return render([[
        {
            $CSTART
            $CFINISH
            $CINC
            for($CDECL = _forstart; $CCMP; $CSTEP) {
                $CBLOCK
                $CHECKGC
            }
        }
    ]], {
        CSTART = cstart,
        CFINISH = cfinish,
        CINC = cinc,
        CDECL = cdecl,
        CCMP = ccmp,
        CSTEP = cstep,
        CBLOCK = cblock,
        CHECKGC = nallocs > 0 and "luaC_checkGC(L);" or ""
    })
end

local function get_table_key(keytype, t, k, slot, newkey)
    local tgetkey
    if keytype._tag == "Type.String" then
        tgetkey = [[
            if (ttype($K) == LUA_TSHRSTR) {
                $SLOT = (TValue *) luaH_getshortstr($T, tsvalue($K));
            } else {
                $SLOT = (TValue *) getgeneric($T, $K);
            }
        ]]
    elseif keytype._tag == "Type.Integer" then
        tgetkey = [[
            $SLOT = (TValue *) luaH_getint($T, ivalue($K));
        ]]
    elseif keytype._tag == "Type.Float" then
        tgetkey = [[
            {
                lua_Integer _ik;
                if (luaV_tointeger($K, &_ik, 0)) {
                    $SLOT = (TValue *) luaH_getint($T, _ik);
                } else {
                    $SLOT = (TValue *) getgeneric($T, $K);
                }
            }
        ]]
    else
        tgetkey = [[
            $SLOT = (TValue *) getgeneric($T, $K);
        ]]
    end
    if newkey then
        tgetkey = tgetkey .. [[
            if ($SLOT == luaO_nilobject) {
                /* create new entry if no previous one */
                $SLOT = luaH_newkey(L, $T, $K);
            }
        ]]
    end
    return render(tgetkey, {
        SLOT = slot,
        T = t,
        K = k,
    })
end

local function codeassignment(ctx, var, cexp, etype)
    local vtag = var._tag
    if vtag == "Ast.VarName" or (vtag == "Ast.VarDot" and var._decl) then
        if vtag == "Ast.VarDot" and var._decl._tag == "Type.Field" then
            local lstats, lexp = codeexp(ctx, var.exp)
            local index = var._decl.index
            local slot = render([[&(($RECORD)->upvalue[$INDEX])]], {
                RECORD = lexp,
                INDEX = index
            })
            return render([[
                $STATS
                $SETSLOT
            ]], {
                STATS = lstats,
                SETSLOT = setslot(var._type, slot, cexp)
            })
        elseif vtag == "Ast.VarDot" then
            ctx.variables[var.exp.var.name .. "." .. var.name] = {
                module = var.exp.var.name,
                name = var.name,
                slot = var._decl._slot
            }
            return setslot(var._type, var._decl._slot, cexp)
        elseif var._decl._tag == "Ast.TopLevelVar" and not var._decl.islocal then
            return setslot(var._type, var._decl._slot, cexp)
        elseif var._decl._used then
            local cset = ""
            if types.is_gc(var._type) then
                cset = setslot(var._type, var._decl._slot, var._decl._cvar)
            end
            return render([[
                $CVAR = $CEXP;
                $CSET
            ]], {
                CVAR = var._decl._cvar,
                CEXP = cexp,
                CSET = cset,
            })
        else
            return render([[
                ((void)$CEXP);
            ]], {
                CEXP = cexp,
            })
        end
    elseif vtag == "Ast.VarBracket" then
        local exp1type = var.exp1._type._tag
        if exp1type == "Type.Array" then
            local arr = var.exp1
            local idx = var.exp2
            local castats, caexp = codeexp(ctx, arr)
            local cistats, ciexp = codeexp(ctx, idx)
            local cset
            if types.is_gc(arr._type.elem) then
                -- write barrier
                cset = render([[
                    TValue _vv;
                    $SETSLOT
                    setobj2t(L, _slot, &_vv);
                    luaC_barrierback(L, _t, &_vv);
                ]], {
                    SETSLOT = setwrapped(etype, "&_vv", cexp)
                })
            else
                cset = setslot(etype, "_slot", cexp)
            end
            return render([[
                {
                    $CASTATS
                    $CISTATS
                    Table *_t = $CAEXP;
                    lua_Integer _k = $CIEXP;
                    unsigned int _actual_i = l_castS2U(_k) - 1;
                    unsigned int _asize = _t->sizearray;
                    TValue *_slot;
                    if (_actual_i < _asize) {
                        _slot = &_t->array[_actual_i];
                    } else if (_actual_i < 2*_asize) {
                        unsigned int _hsize = sizenode(_t);
                        luaH_resize(L, _t, 2*_asize, _hsize);
                        _slot = &_t->array[_actual_i];
                    } else {
                        _slot = (TValue *)luaH_getint(_t, _k);
                        TValue _vk; setivalue(&_vk, _k);
                        if (_slot == luaO_nilobject) {
                            /* create new entry if no previous one */
                            _slot = luaH_newkey(L, _t, &_vk);
                        }
                    }
                    $CSET
                }
            ]], {
                CASTATS = castats,
                CISTATS = cistats,
                CAEXP = caexp,
                CIEXP = ciexp,
                CSET = cset
            })
        elseif exp1type == "Type.Map" then
            local map = var.exp1
            local key = var.exp2
            local cmstats, cmexp = codeexp(ctx, map)
            local ckstats, ckexp = codeexp(ctx, key)
            local cset
            if types.is_gc(map._type.values) then
                -- write barrier
                cset = render([[
                    TValue _vv;
                    $SETSLOT
                    setobj2t(L, _slot, &_vv);
                    luaC_barrierback(L, _t, &_vv);
                ]], {
                    SETSLOT = setwrapped(etype, "&_vv", cexp)
                })
            else
                cset = setslot(etype, "_slot", cexp)
            end

            local ctmpk, tmpkname, tmpkslot = newtmp(ctx, key._type, true)

            return render([[
                {
                    $CMSTATS
                    Table *_t = $CMEXP;
                    $CKSTATS
                    $CTMPK
                    $TMPKNAME = $CKEXP;
                    $SETSLOTK

                    TValue *_slot;
                    $CTABLEKEY
                    $CSET
                }
            ]], {
                CMSTATS = cmstats,
                CMEXP = cmexp,
                CKSTATS = ckstats,
                CTMPK = ctmpk,
                TMPKNAME = tmpkname,
                CKEXP = ckexp,
                SETSLOTK = setwrapped(key._type, tmpkslot, tmpkname),
                CTABLEKEY = get_table_key(key._type, "_t", tmpkslot, "_slot", true),
                CSET = cset,
            })
        end
    else
        error("invalid tag for lvalue of assignment: " .. vtag)
    end
end

local function codesingleassignment(ctx, node)
    local var = node.vars[1]
    local exp = node.exps[1]
    local cstats, cexp = codeexp(ctx, exp, false, var._decl)
    return cstats .. "\n" .. codeassignment(ctx, var, cexp, exp._type)
end

local function codemultiassignment(ctx, node)
    local stats = {}
    local tmps = {}
    for i, exp in ipairs(node.exps) do
        local var = node.vars[i]
        local vtag = var and var._tag
        if var then
            local typ = exp._type
            local ctmp, tmpname = newtmp(ctx, typ)
            tmps[i] = tmpname
            local cstats, cexp = codeexp(ctx, exp)
            table.insert(stats, render([[
                $CTMP
                $CSTATS
                $TMPNAME = $CEXP;
            ]], {
                CTMP = ctmp,
                CSTATS = cstats,
                TMPNAME = tmpname,
                CEXP = cexp
            }))
        end
    end
    for i, var in ipairs(node.vars) do
        if tmps[i] then table.insert(stats, codeassignment(ctx, var, tmps[i], node.exps[i]._type)) end
    end
    return table.concat(stats, "\n")
end

local function makestring(ctx, cexp, target)
    local cstr = render("luaS_new(L, $VALUE)", {
        VALUE = cexp
    })
    if target then
        return "", cstr
    else
        local ctmp, tmpname, tmpslot = newtmp(ctx, types.String(), true)
        return render([[
            $CTMP
            $TMPNAME = $CSTR;
            setsvalue(L, $TMPSLOT, $TMPNAME);
        ]], {
            CTMP = ctmp,
            TMPNAME = tmpname,
            CSTR = cstr,
            TMPSLOT = tmpslot,
        }), tmpname
    end
end

local function nativetoforeignexp(ctx, exp, cexp)
    if exp._type._tag == "Type.Integer"
    or exp._type._tag == "Type.Float"
    or exp._type._tag == "Type.Boolean"
    or exp._type._tag == "Type.Pointer" then
        return "", cexp
    end

    if exp._type._tag == "Type.Nil" then
        return "", "NULL"
    end

    if exp._type._tag == "Type.String" then
        local texp, tmp = newforeigntmp(ctx, exp._type)
        local cvtexp = render([[getstr($CEXP)]], { CEXP = cexp })
        return render([[
            $TEXP
            $TMP = $CVTEXP;
        ]], {
            TEXP = texp,
            TMP = tmp,
            CVTEXP = cvtexp,
        }), tmp
    end

    error("don't know how to handle type "..types.tostring(exp._type))
end

local function foreigntonativeexp(ctx, exp, cexp, target)
    if exp._type._tag == "Type.Integer"
    or exp._type._tag == "Type.Float"
    or exp._type._tag == "Type.Boolean"
    or exp._type._tag == "Type.Pointer" then
        return "", cexp
    end

    if exp._type._tag == "Type.Nil" then
        return "", "0"
    end

    if exp._type._tag == "Type.String" then
        return makestring(ctx, cexp, target)
    end

    if exp._type._tag == "Type.Typedef" then
        return foreigntonativeexp(ctx, exp._type, cexp, target)
    end

    error("don't know how to handle type "..types.tostring(exp._type))
end

local function codeforeigncall(ctx, node)
    -- TODO catch name conflicts among imports
    local cfuncname = node.exp.var.name
    local caexps = {}
    local castats = {}
    for _, arg in ipairs(node.args.args) do
        local cstat, cexp = codeforeignexp(ctx, arg)
        table.insert(castats, cstat)
        table.insert(caexps, cexp)
    end
    local cstats = table.concat(castats, "\n")
    local ccall = render("$NAME($CAEXPS)", {
        NAME = cfuncname,
        CAEXPS = table.concat(caexps, ", "),
    })
    return cstats, ccall
end

local function codeforeignvar(ctx, node)
    return "", node.name
end

local function codecall(ctx, node)
    local castats, caexps, tmpnames, retslots = {}, { "L" }, {}, {}
    local fname
    local fnode = node.exp.var
    if node.args._tag == "Ast.ArgsFunc" then
        if fnode._tag == "Ast.VarName" then
            fname = func_name(ctx.module, fnode.name)
        elseif fnode._tag == "Ast.VarDot" then
            if fnode.exp._type._tag == "Type.ForeignModule" then
                return codeforeigncall(ctx, node)
            elseif fnode._decl._tag == "Type.StaticMethod" then
                fname = static_name(fnode._decl)
                local mod, record = fnode._decl.fqtn:match("^(.*)%.(%a+)$")
                if mod ~= ctx.module then
                    ctx.functions[fnode._decl.fqtn .. "." .. fnode._decl.name] = {
                        module = mod,
                        record = record,
                        type = fnode._decl,
                        name = fnode._decl.name,
                        mangled = fname
                    }
                end
            else
                assert(fnode._decl._tag == "Type.ModuleMember")
                fname = func_name(fnode._decl.modname, fnode.name)
                local mod = fnode._decl.modname
                if mod ~= ctx.module then
                    ctx.functions[mod .. "." .. fnode.name] = {
                        module = mod,
                        name = fnode.name,
                        type = fnode._type,
                        mangled = fname
                    }
                end
            end
        end
    else
        assert(node.args._tag == "Ast.ArgsMethod")
        local mtype = node._method
        fname = method_name(mtype)
        local mod, record = mtype.fqtn:match("^(.*)%.(%a+)$")
        if mod ~= ctx.module then
            ctx.functions[mtype.fqtn .. ":" .. mtype.name] = {
                module = mod,
                record = record,
                name = mtype.name,
                type = mtype,
                mangled = fname
            }
        end
        local cstat, cexp = codeexp(ctx, node.exp)
        table.insert(castats, cstat)
        table.insert(caexps, cexp)
    end
    for _, arg in ipairs(node.args.args) do
        local cstat, cexp = codeexp(ctx, arg)
        table.insert(castats, cstat)
        table.insert(caexps, cexp)
    end
    for i = 2, #node._types do
        node._extras = node._extras or {}
        local typ = node._types[i]
        local ctmp, tmpname, tmpslot = newtmp(ctx, typ, types.is_gc(typ))
        node._extras[i] = tmpname
        tmpnames[i] = tmpname
        table.insert(castats, ctmp)
        table.insert(caexps, "&" .. tmpname)
        retslots[i] = tmpslot
    end
    local cstats = table.concat(castats, "\n")
    local ccall = render("$NAME($CAEXPS)", {
        NAME = fname,
        CAEXPS = table.concat(caexps, ", "),
    })
    if util.any(types.is_gc, node._types) then
        local ctmp, tmpname, tmpslot = newtmp(ctx, node._type, types.is_gc(node._type))
        tmpnames[1] = tmpname
        retslots[1] = tmpslot
        local cslots = {}
        for i, typ in ipairs(node._types) do
            if types.is_gc(typ) then
                table.insert(cslots, setslot(typ, retslots[i], tmpnames[i]) .. ";")
            end
        end
        return render([[
            $CSTATS
            $CTMP
            $TMPNAME = $CCALL;
            $SLOTS
        ]], {
            CSTATS = cstats,
            CTMP = ctmp,
            TMPNAME = tmpname,
            CCALL = ccall,
            SLOTS = table.concat(cslots, "\n"),
        }), tmpname
    else
        return cstats, ccall
    end
end

local function codereturn(ctx, node)
    local cs, cexp = codeexp(ctx, node.exps[1])
    local cstats = { cs }
    for i = 2, #node.exps do
        local cs, ce = codeexp(ctx, node.exps[i])
        table.insert(cstats, cs)
        table.insert(cstats, "*_outparam_" .. i .. " = " .. ce .. ";")
    end
    local tmpl
    if ctx.nslots > 0 then
        tmpl = [[
            $CSTATS
            L->top = _base;
            return $CEXP;
        ]]
    else
        tmpl = [[
            $CSTATS
            return $CEXP;
        ]]
    end
    return render(tmpl, {
        CSTATS = table.concat(cstats, "\n"),
        CEXP = cexp,
    })
end

local function localname(ctx, name)
    local idx = ctx.names[name] or 1
    ctx.names[name] = idx + 1
    return name .. "_" .. tostring(idx)
end

function codestat(ctx, node)
    local tag = node._tag
    if tag == "Ast.StatDecl" then
        local code = {}
        for i = 1, #node.decls do
            local exp = node.exps[i]
            local decl = node.decls[i]
            local cstats, cexp = codeexp(ctx, exp)
            if decl._used then
                local typ = decl._type
                decl._cvar = "_local_" .. localname(ctx, decl.name)
                local cdecl = ctype(typ) .. " " .. decl._cvar .. ";"
                local cslot = ""
                local cset = ""
                if types.is_gc(typ) then
                    decl._slot = "_localslot_" .. localname(ctx, decl.name)
                    cslot = newslot(ctx, decl._slot);
                    cset = render([[
                        /* update slot */
                        $SETSLOT
                    ]], {
                        SETSLOT = setslot(typ, decl._slot, decl._cvar),
                    })
                end
                table.insert(code, render([[
                    $CDECL
                    $CSLOT
                    $CSTATS
                    $CVAR = $CEXP;
                    $CSET
                ]], {
                    CDECL = cdecl,
                    CSLOT = cslot,
                    CSTATS = cstats,
                    CVAR = decl._cvar,
                    CEXP = cexp,
                    CSET = cset
                }))
            else
                table.insert(code, render([[
                    $CSTATS
                    ((void)$CEXP);
                ]], {
                    CSTATS = cstats,
                    CEXP = cexp
                }))
            end
        end
        return table.concat(code, "\n")
    elseif tag == "Ast.StatBlock" then
        return codeblock(ctx, node)
    elseif tag == "Ast.StatWhile" then
        return codewhile(ctx, node)
    elseif tag == "Ast.StatRepeat" then
        return coderepeat(ctx, node)
    elseif tag == "Ast.StatIf" then
        return codeif(ctx, node)
    elseif tag == "Ast.StatFor" then
        return codefor(ctx, node)
    elseif tag == "Ast.StatAssign" then
        if #node.vars == 1 then
            return codesingleassignment(ctx, node)
        else
            return codemultiassignment(ctx, node)
        end
    elseif tag == "Ast.StatCall" then
        if node.callexp.exp.var._tag == "Ast.VarDot"
           and node.callexp.exp.var.exp._type._tag == "Type.ForeignModule" then
            local fstats, fexp = codeforeigncall(ctx, node.callexp)
            return fstats .. fexp .. ";"
        else
            local cstats, cexp = codecall(ctx, node.callexp)
            return cstats .. "\n    " .. cexp .. ";"
        end
    elseif tag == "Ast.StatReturn" then
        return codereturn(ctx, node)
    else
        error("invalid node tag " .. tag)
    end
end

-- All the code generation functions for EXPRESSIONS return
-- preliminary C code necessary for computing the expression
-- as a string of C statements, plus the code for the expression
-- as a string with a C expression. For trivial expressions
-- the preliminary code is always the empty string

local function codevar(ctx, node)
    if node._tag == "Ast.VarDot" then
        if node._decl._tag == "Type.Field" then
            local cstats, cexp = codeexp(ctx, node.exp)
            return cstats,
                getslot(node._type, nil, render("&(($EXP)->upvalue[$INDEX])", {
                    EXP = cexp,
                    INDEX = node._decl.index
                }))
        else
            assert(node._decl._tag == "Type.ModuleMember")
            ctx.variables[node.exp.var.name .. "." .. node.name] = {
                module = node.exp.var.name,
                name = node.name,
                slot = node._decl._slot
            }
            return "", getslot(node._type, nil, node._decl._slot)
        end
    elseif node._decl._tag == "Ast.TopLevelVar" and not node._decl.islocal then
        return "", getslot(node._type, nil, node._decl._slot)
    else
        return "", node._decl._cvar
    end
end

local function codevalue(ctx, node, target)
    local tag = node._tag
    if tag == "Ast.ExpNil" then
        return "", "0"
    elseif tag == "Ast.ExpBool" then
        return "", node.value and "1" or "0"
    elseif tag == "Ast.ExpInteger" then
        return "", c_integer_literal(node.value)
    elseif tag == "Ast.ExpFloat" then
        return "", c_float_literal(node.value)
    elseif tag == "Ast.ExpString" then
        return makestring(ctx, c_string_literal(node.value), target)
    else
        error("invalid tag for a literal value: " .. tag)
    end
end

local function codearray(ctx, node, target)
    local stats = {}
    local cinit, ctmp, tmpname, tmpslot
    if target then
        ctmp, tmpname, tmpslot = "", target._cvar, target._slot
    else
        ctmp, tmpname, tmpslot = newtmp(ctx, node._type, true)
    end
    cinit = render([[
        $CTMP
        $TMPNAME = luaH_new(L);
        sethvalue(L, $TMPSLOT, $TMPNAME);
    ]], {
        CTMP = ctmp,
        TMPNAME = tmpname,
        TMPSLOT = tmpslot,
    })
    table.insert(stats, cinit)
    local slots = {}
    for _, field in ipairs(node.fields) do
        local exp = field.exp
        local cstats, cexp = codeexp(ctx, exp)
        local ctmpe, tmpename, tmpeslot = newtmp(ctx, node._type.elem, true)

        local code = render([[
            $CSTATS
            $CTMPE
            $TMPENAME = $CEXP;
            $SETSLOT
        ]], {
            CSTATS = cstats,
            CTMPE = ctmpe,
            TMPENAME = tmpename,
            CEXP = cexp,
            SETSLOT = setwrapped(node._type.elem, tmpeslot, tmpename),
        })

        table.insert(slots, tmpeslot)
        table.insert(stats, code)
    end
    if #node.fields > 0 then
        table.insert(stats, render([[
            luaH_resizearray(L, $TMPNAME, $SIZE);
        ]], {
            TMPNAME = tmpname,
            SIZE = #node.fields
        }))

    end
    for i, slot in ipairs(slots) do
        table.insert(stats, render([[
            setobj2t(L, &$TMPNAME->array[$INDEX], $SLOT);
        ]], {
            TMPNAME = tmpname,
            INDEX = i-1,
            SLOT = slot
        }))
        if types.is_gc(node._type.elem) then
            table.insert(stats, render([[
                luaC_barrierback(L, $TMPNAME, $SLOT);
            ]], {
                TMPNAME = tmpname,
                SLOT = slot,
            }))
        end
    end
    return table.concat(stats, "\n"), tmpname
end

local function codemap(ctx, node, target)
    local stats = {}
    local cinit, ctmp, tmpname, tmpslot
    if target then
        ctmp, tmpname, tmpslot = "", target._cvar, target._slot
    else
        ctmp, tmpname, tmpslot = newtmp(ctx, node._type, true)
    end
    cinit = render([[
        $CTMP
        $TMPNAME = luaH_new(L);
        sethvalue(L, $TMPSLOT, $TMPNAME);
    ]], {
        CTMP = ctmp,
        TMPNAME = tmpname,
        TMPSLOT = tmpslot,
    })
    table.insert(stats, cinit)
    local slots = {}
    for _, field in ipairs(node.fields) do
        local kexp = field.name
        local ckstats, ckexp = codeexp(ctx, kexp)
        local ctmpk, tmpkname, tmpkslot = newtmp(ctx, node._type.keys, true)

        local vexp = field.exp
        local cvstats, cvexp = codeexp(ctx, vexp)
        local ctmpv, tmpvname, tmpvslot = newtmp(ctx, node._type.values, true)

        local code = render([[
            $CKSTATS
            $CTMPK
            $TMPKNAME = $CKEXP;
            $SETSLOTK

            $CVSTATS
            $CTMPV
            $TMPVNAME = $CVEXP;
            $SETSLOTV
        ]], {
            CKSTATS = ckstats,
            CTMPK = ctmpk,
            TMPKNAME = tmpkname,
            CKEXP = ckexp,
            SETSLOTK = setwrapped(node._type.keys, tmpkslot, tmpkname),

            CVSTATS = cvstats,
            CTMPV = ctmpv,
            TMPVNAME = tmpvname,
            CVEXP = cvexp,
            SETSLOTV = setwrapped(node._type.values, tmpvslot, tmpvname),
        })

        table.insert(slots, { k = tmpkslot, v = tmpvslot })
        table.insert(stats, code)
    end
    for _, slot in ipairs(slots) do
        table.insert(stats, render([[
            {
                TValue *_slot;
                $CTABLEKEY
                setobj2t(L, _slot, $SLOT);
            }
        ]], {
            CTABLEKEY = get_table_key(node._type.keys, tmpname, slot.k, "_slot", true),
            SLOT = slot.v,
        }))
        if types.is_gc(node._type.keys) then
            table.insert(stats, render([[
                luaC_barrierback(L, $TMPNAME, $SLOTK);
            ]], {
                TMPNAME = tmpname,
                SLOTK = slot.k,
            }))
        end
        if types.is_gc(node._type.values) then
            table.insert(stats, render([[
                luaC_barrierback(L, $TMPNAME, $SLOTV);
            ]], {
                TMPNAME = tmpname,
                SLOTV = slot.v,
            }))
        end
    end
    return table.concat(stats, "\n"), tmpname
end

local function coderecord(ctx, node, target)
    local stats = {}
    local cinit, ctmp, tmpname, tmpslot
    local record = types.registry[node._type.fqtn]
    if target then
        ctmp, tmpname, tmpslot = "", target._cvar, target._slot
    else
        ctmp, tmpname, tmpslot = newtmp(ctx, node._type, true)
    end
    cinit = render([[
        $CTMP
        $TMPNAME = luaF_newCclosure(L, $NFIELDS);
        {
            Udata* _ud = luaS_newudata(L, sizeof(ptrdiff_t));
            *((ptrdiff_t*)(getudatamem(_ud))) = $TAG;
            _ud->metatable = $META;
            _ud->user_.gc = (GCObject*)$TMPNAME;
            _ud->ttuv_ = ctb(LUA_TCCL);
            setuvalue(L, &($TMPNAME->upvalue[0]), _ud);
        }
        setclCvalue(L, $TMPSLOT, $TMPNAME);
    ]], {
        CTMP = ctmp,
        TMPNAME = tmpname,
        TMPSLOT = tmpslot,
        NFIELDS = #record.fields + 1,
        TAG = type2tagname(ctx, node._type),
        META = type2metatable(ctx, node._type)
    })
    table.insert(stats, cinit)
    local slots = {}
    for _, field in ipairs(node.fields) do
        local exp = field.exp
        local cstats, cexp = codeexp(ctx, exp)
        local ctmpe, tmpename, tmpeslot = newtmp(ctx, field._field.type, true)
        local code = render([[
            $CSTATS
            $CTMPE
            $TMPENAME = $CEXP;
            $SETSLOT;
            setobj2t(L, &($RECORD->upvalue[$INDEX]), $SLOT);
        ]], {
            CSTATS = cstats,
            CTMPE = ctmpe,
            TMPENAME = tmpename,
            CEXP = cexp,
            SETSLOT = setslot(field._field.type, tmpeslot, tmpename),
            SLOT = tmpeslot,
            RECORD = tmpname,
            INDEX = field._field.index
        })
        table.insert(stats, code)
        if types.is_gc(field._field.type) then
            table.insert(stats, render([[
                luaC_barrier(L, $TMPNAME, $SLOT);
            ]], {
                TMPNAME = tmpname,
                SLOT = tmpeslot,
            }))
        end
    end
    return table.concat(stats, "\n"), tmpname
end

local function codeunaryop(ctx, node, iscondition)
    local op = node.op
    if op == "not" then
        local estats, ecode = codeexp(ctx, node.exp, iscondition)
        return estats, "!(" .. ecode .. ")"
    elseif op == "#" then
        local estats, ecode = codeexp(ctx, node.exp)
        if node.exp._type._tag == "Type.Array" then
            return estats, "luaH_getn(" .. ecode .. ")"
        else
            return estats, "tsslen(" .. ecode .. ")"
        end
    else
        local estats, ecode = codeexp(ctx, node.exp)
        return estats, "(" .. op .. ecode .. ")"
    end
end

-- In Lua and Titan, the shift amount in a bitshift can be any integer, but in
-- C shift amount must be a positive number less than the width of the integer
-- type being shifted. This means that we need some if statements to implement
-- the Lua shift semantics in C.
--
-- Most of the time, the shift amount should be a constant, which will allow the
-- C compiler to eliminate all of these branches as dead code and generate code that is
-- just as good as a raw C bitshift without the extra Lua semantics.
--
-- For the dynamic case, we gain a bit of performance (~20%) compared to the
-- algorithm in luaV_shiftl by reordering the branches to put the common case
-- (shift amount is a small positive integer) under only one level of branching
-- and with a TITAN_LIKELY annotation.
local function generate_binop_shift(shift_pos, shift_neg, exp, ctx)
    local x_stats, x_var = codeexp(ctx, exp.lhs)
    local y_stats, y_var = codeexp(ctx, exp.rhs)
    local rdecl, rname = newtmp(ctx, types.Integer())
    local cstats = util.render([[
        ${X_STATS}
        ${Y_STATS}
        ${R_DECL}
        if (TITAN_LIKELY(l_castS2U(${Y}) < TITAN_LUAINTEGER_NBITS)) {
            ${R} = intop(${SHIFT_POS}, ${X}, ${Y});
        } else {
            if (l_castS2U(-${Y}) < TITAN_LUAINTEGER_NBITS) {
                ${R} = intop(${SHIFT_NEG}, ${X}, -${Y});
            } else {
                ${R} = 0;
            }
        }
    ]], {
        SHIFT_POS = shift_pos,
        SHIFT_NEG = shift_neg,
        X = x_var,
        X_STATS = x_stats,
        Y = y_var,
        Y_STATS = y_stats,
        R = rname,
        R_DECL = rdecl,
    })
    return cstats, rname
end

-- Lua/Titan integer division rounds to negative infinity instead of towards
-- zero. We inline luaV_div here to give the C compiler more optimization
-- opportunities (see that function for comments on how it works).
local function generate_binop_idiv_int(exp, ctx)
    local m_stats, m_var = codeexp(ctx, exp.lhs)
    local n_stats, n_var = codeexp(ctx, exp.rhs)
    local qdecl, qname = newtmp(ctx, exp._type)
    local cstats = util.render([[
        ${M_STATS}
        ${N_STATS}
        ${Q_DECL}
        if (l_castS2U(${N}) + 1u <= 1u) {
            if (${N} == 0){
                luaL_error(L, "error at line %d, divide by zero", $LINE);
            } else {
                ${Q} = intop(-, 0, ${M});
            }
        } else {
            ${Q} = ${M} / ${N};
            if ((${M} ^ ${N}) < 0 && ${M} % ${N} != 0) {
                ${Q} -= 1;
            }
        }
    ]], {
        M = m_var,
        M_STATS = m_stats,
        N = n_var,
        N_STATS = n_stats,
        Q = qname,
        Q_DECL = qdecl,
        LINE = c_integer_literal(exp.loc.line),
    })
    return cstats, qname
end

-- see luai_numidiv
local function generate_binop_idiv_flt(exp, ctx)
    local x_stats, x_var = codeexp(ctx, exp.lhs)
    local y_stats, y_var = codeexp(ctx, exp.rhs)
    local rdecl, rname = newtmp(ctx, exp._type)
    local cstats = util.render([[
        ${X_STATS}
        ${Y_STATS}
        ${R_DECL}
        ${R_NAME} = floor(${X} / ${Y});
    ]], {
        X = x_var,
        X_STATS = x_stats,
        Y = y_var,
        Y_STATS = y_stats,
        R_DECL = rdecl,
        R_NAME = rname,
    })
    return cstats, rname
end

-- Lua/Titan guarantees that (m == n*(m//n) + (,%n))
-- See generate binop_intdiv and luaV_mod
local function generate_binop_mod_int(exp, ctx)
    local m_stats, m_var = codeexp(ctx, exp.lhs)
    local n_stats, n_var = codeexp(ctx, exp.rhs)
    local rdecl, rname = newtmp(ctx, types.Integer())
    local cstats = util.render([[
        ${M_STATS}
        ${N_STATS}
        ${R_DECL};
        if (l_castS2U(${N}) + 1u <= 1u) {
            if (${N} == 0){
                luaL_error(L, "error at line %d, % by zero", $LINE);
            } else {
                ${R} = 0;
            }
        } else {
            ${R} = ${M} % ${N};
            if (${R} != 0 && (${M} ^ ${N}) < 0) {
                ${R} += ${N};
            }
        }
    ]], {
        M = m_var,
        M_STATS = m_stats,
        N = n_var,
        N_STATS = n_stats,
        R = rname,
        R_DECL = rdecl,
        LINE = c_integer_literal(exp.loc.line),
    })
    return cstats, rname
end

local function codebinaryop(ctx, node, iscondition)
    local op = node.op
    if op == "~=" then op = "!=" end
    if op == "and" then
        local lstats, lcode = codeexp(ctx, node.lhs, iscondition)
        local rstats, rcode = codeexp(ctx, node.rhs, iscondition)
        if lstats == "" and rstats == "" and iscondition then
            return "", "(" .. lcode .. " && " .. rcode .. ")"
        else
            local ctmp, tmpname, tmpslot = newtmp(ctx, node._type, types.is_gc(node._type))
            local tmpset = types.is_gc(node._type) and setslot(node._type, tmpslot, tmpname) or ""
            local code = render([[
                $LSTATS
                $CTMP
                $TMPNAME = $LCODE;
                if($TMPNAME) {
                  $RSTATS
                  $TMPNAME = $RCODE;
                }
                $TMPSET;
            ]], {
                CTMP = ctmp,
                TMPNAME = tmpname,
                LSTATS = lstats,
                LCODE = lcode,
                RSTATS = rstats,
                RCODE = rcode,
                TMPSET = tmpset,
            })
            return code, tmpname
        end
    elseif op == "or" then
        local lstats, lcode = codeexp(ctx, node.lhs, true)
        local rstats, rcode = codeexp(ctx, node.rhs, iscondition)
        if lstats == "" and rstats == "" and iscondition then
            return "", "(" .. lcode .. " || " .. rcode .. ")"
        else
            local ctmp, tmpname, tmpslot = newtmp(ctx, node._type, types.is_gc(node._type))
            local tmpset = types.is_gc(node._type) and setslot(node._type, tmpslot, tmpname) or ""
            local code = render([[
                $LSTATS
                $CTMP
                $TMPNAME = $LCODE;
                if(!$TMPNAME) {
                    $RSTATS;
                    $TMPNAME = $RCODE;
                }
                $TMPSET;
            ]], {
                CTMP = ctmp,
                TMPNAME = tmpname,
                LSTATS = lstats,
                LCODE = lcode,
                RSTATS = rstats,
                RCODE = rcode,
                TMPSET = tmpset,
            })
            return code, tmpname
        end
    elseif op == "^" then
        local lstats, lcode = codeexp(ctx, node.lhs)
        local rstats, rcode = codeexp(ctx, node.rhs)
        return lstats .. rstats, "pow(" .. lcode .. ", " .. rcode .. ")"
    elseif op == "<<" then
        return generate_binop_shift("<<", ">>", node, ctx)
    elseif op == ">>" then
        return generate_binop_shift(">>", "<<", node, ctx)
    elseif op == "%" then
        local ltyp = node.lhs._type._tag
        local rtyp = node.rhs._type._tag
        if ltyp == "Type.Integer" and rtyp == "Type.Integer" then
            return generate_binop_mod_int(node, ctx)
        elseif ltyp == "Type.Float" and rtyp == "Type.Float" then
            -- see luai_nummod
            error("not implemented yet")
        else
            error("impossible: " .. ltyp .. " " .. rtyp)
        end
    elseif op == "//" then
        local ltyp = node.lhs._type._tag
        local rtyp = node.rhs._type._tag
        if ltyp == "Type.Integer" and rtyp == "Type.Integer" then
            return generate_binop_idiv_int(node, ctx)
        elseif ltyp == "Type.Float" and rtyp == "Type.Float" then
            return generate_binop_idiv_flt(node, ctx)
        else
            error("impossible: " .. ltyp .. " " .. rtyp)
        end
    else
        local lstats, lcode = codeexp(ctx, node.lhs)
        local rstats, rcode = codeexp(ctx, node.rhs)
        return lstats .. rstats, "(" .. lcode .. op .. rcode .. ")"
    end
end

local function codeindexarray(ctx, node, iscondition)
    local castats, caexp = codeexp(ctx, node.exp1)
    local cistats, ciexp = codeexp(ctx, node.exp2)
    local typ = node._type
    local ctmp, tmpname, tmpslot = newtmp(ctx, typ, types.is_gc(typ))
    local cset = ""
    local ccheck = checkandget(ctx, typ, tmpname, "_s", node.loc)
    if types.is_gc(typ) then
        cset = setslot(typ, tmpslot, tmpname)
    end
    local cfinish
    if iscondition then
        cfinish = render([[
          if(ttisnil(_s)) {
            $TMPNAME = 0;
          } else {
            $CCHECK
            $CSET
          }
        ]], {
            TMPNAME = tmpname,
            CCHECK = ccheck,
            CSET = cset,
        })
    else
        cfinish = render([[
            $CCHECK
            $CSET
        ]], {
            CCHECK = ccheck,
            CSET = cset,
        })
    end
    local stats = render([[
        $CTMP
        {
            $CASTATS
            $CISTATS
            Table *_t = $CAEXP;
            lua_Integer _k = $CIEXP;

            unsigned int _actual_i = l_castS2U(_k) - 1;

            const TValue *_s;
            if (_actual_i < _t->sizearray) {
                _s = &_t->array[_actual_i];
            } else {
                _s = luaH_getint(_t, _k);
            }

            $CFINISH
    }]], {
        CTMP = ctmp,
        CASTATS = castats,
        CISTATS = cistats,
        CAEXP = caexp,
        CIEXP = ciexp,
        CFINISH = cfinish
    })
    return stats, tmpname
end

local function codeindexmap(ctx, node, iscondition)
    local cmstats, cmexp = codeexp(ctx, node.exp1)
    local ckstats, ckexp = codeexp(ctx, node.exp2)
    local typ = node._type
    local ctmp, tmpname, tmpslot = newtmp(ctx, typ, types.is_gc(typ))
    local cset = ""
    local ccheck = checkandget(ctx, typ, tmpname, "_s", node.loc)
    if types.is_gc(typ) then
        cset = setslot(typ, tmpslot, tmpname)
    end
    local cfinish
    if iscondition then
        cfinish = render([[
          if(ttisnil(_s)) {
            $TMPNAME = 0;
          } else {
            $CCHECK
            $CSET
          }
        ]], {
            TMPNAME = tmpname,
            CCHECK = ccheck,
            CSET = cset,
        })
    else
        cfinish = render([[
            $CCHECK
            $CSET
        ]], {
            CCHECK = ccheck,
            CSET = cset,
        })
    end
    local ktyp = node.exp2._type
    local ctmpk, tmpkname, tmpkslot = newtmp(ctx, ktyp, true)
    local stats = render([[
        $CTMP
        {
            $CMSTATS
            $CKSTATS
            Table *_t = $CMEXP;
            $CTMPK
            $TMPKNAME = $CKEXP;
            $SETSLOTK
            const TValue* _s;
            $CTABLEKEY
            $CFINISH
    }]], {
        CTMP = ctmp,
        CMSTATS = cmstats,
        CKSTATS = ckstats,
        CMEXP = cmexp,
        CTMPK = ctmpk,
        TMPKNAME = tmpkname,
        CKEXP = ckexp,
        SETSLOTK = setwrapped(ktyp, tmpkslot, tmpkname),
        CTABLEKEY = get_table_key(ktyp, "_t", tmpkslot, "_s", false),
        CFINISH = cfinish
    })
    return stats, tmpname
end

-- Generate code for expression 'node'
-- 'iscondition' is 'true' if expression is used not for value but for
--    controlling conditinal execution
-- 'target' is not nil if expression is rvalue for a 'Ast.VarName' lvalue,
--    in this case it will be the '_decl' of the lvalue
function codeexp(ctx, node, iscondition, target)
    local tag = node._tag
    if tag == "Ast.VarDot" and node.exp._type._tag == "Type.ForeignModule" then
        local fstats, fexp = codeforeignvar(ctx, node)
        local cstats, cexp = foreigntonativeexp(ctx, node, fexp, target)
        return fstats .. cstats, cexp
    elseif tag == "Ast.VarName" or (tag == "Ast.VarDot" and node._decl) then
        return codevar(ctx, node)
    elseif tag == "Ast.VarBracket" then
        if node.exp1._type._tag == "Type.Array" then
            return codeindexarray(ctx, node, iscondition)
        elseif node.exp1._type._tag == "Type.Map" then
            return codeindexmap(ctx, node, iscondition)
        end
        error("impossible var bracket")
    elseif tag == "Ast.ExpNil" or
                tag == "Ast.ExpBool" or
                tag == "Ast.ExpInteger" or
                tag == "Ast.ExpFloat" or
                tag == "Ast.ExpString" then
        return codevalue(ctx, node, target)
    elseif tag == "Ast.ExpInitList" then
        if node._type._tag == "Type.Nominal" then
            return coderecord(ctx, node, target)
        elseif node._type._tag == "Type.Map" then
            return codemap(ctx, node, target)
        else
            return codearray(ctx, node, target)
        end
        error("impossible init list")
    elseif tag == "Ast.ExpVar" then
        return codeexp(ctx, node.var, iscondition)
    elseif tag == "Ast.ExpUnop" then
        return codeunaryop(ctx, node, iscondition)
    elseif tag == "Ast.ExpBinop" then
        return codebinaryop(ctx, node, iscondition)
    elseif tag == "Ast.ExpCall"
           and node.exp.var._tag == "Ast.VarDot"
           and node.exp.var.exp._type._tag == "Type.ForeignModule" then
        local fstats, fexp = codeforeigncall(ctx, node)
        local cstats, cexp = foreigntonativeexp(ctx, node, fexp, target)
        return fstats .. cstats, cexp
    elseif tag == "Ast.ExpCall" then
        return codecall(ctx, node, target)
    elseif tag == "Ast.ExpExtra" then
        return "", node.exp._extras[node.index]
    elseif tag == "Ast.ExpCast" and node.exp._tag == "Ast.ExpVar" and node.exp.var._tag == "Ast.VarBracket" then
        local t = node.exp.var._type
        node.exp.var._type = node.target
        local cstats, cexp = codeexp(ctx, node.exp.var, iscondition)
        node.exp.var._type = t
        return cstats, cexp
    elseif tag == "Ast.ExpCast" and node.exp._type._tag == "Type.Value" then
        local cstats, cexp = codeexp(ctx, node.exp, iscondition)
        local ctmps, tmpnames = newtmp(ctx, node.exp._type)
        local ctmpt, tmpnamet = newtmp(ctx, node.target)
        local cget = checkandget(ctx, node._type, tmpnamet, "&" .. tmpnames, node.loc)
        return render([[
            $EXPSTATS
            $TMPSOURCE
            $SOURCE = $EXP;
            $TMPTARGET
            $CHECKANDGET
        ]], {
            EXPSTATS = cstats,
            TMPSOURCE = ctmps,
            SOURCE = tmpnames,
            TMPTARGET = ctmpt,
            EXP = cexp,
            CHECKANDGET = cget
        }), tmpnamet
    elseif tag == "Ast.ExpCast" and node.target._tag == "Type.Value" then
        local cstats, cexp = codeexp(ctx, node.exp, iscondition)
        local ctmp, tmpname = newtmp(ctx, node.target)
        return render([[
            $EXPSTATS
            $TMPTARGET
            $SETSLOT
        ]], {
            EXPSTATS = cstats,
            TMPTARGET = ctmp,
            SETSLOT = setwrapped(node.exp._type, "&" .. tmpname, cexp)
        }), tmpname
    elseif tag == "Ast.ExpCast" and node.target._tag == "Type.Float" then
        local cstat, cexp = codeexp(ctx, node.exp)
        return cstat, "((lua_Number)" .. cexp .. ")"
    elseif tag == "Ast.ExpCast" and node.target._tag == "Type.Boolean" then
        local cstat, cexp = codeexp(ctx, node.exp, true)
        return cstat, "((" .. cexp .. ") ? 1 : 0)"
    elseif tag == "Ast.ExpCast" and node.target._tag == "Type.Integer" then
        local cstat, cexp = codeexp(ctx, node.exp)
        local ctmp1, tmpname1 = newtmp(ctx, types.Float())
        local ctmp2, tmpname2 = newtmp(ctx, types.Float())
        local ctmp3, tmpname3 = newtmp(ctx, types.Integer())
        local cfloor = render([[
            $CSTAT
            $CTMP1
            $CTMP2
            $CTMP3
            $TMPNAME1 = $CEXP;
            $TMPNAME2 = l_floor($TMPNAME1);
            if ($TMPNAME1 != $TMPNAME2) {
                luaL_error(L, "type error at line %d, number '%f' has no integer representation", $LINE, $TMPNAME1);
            } else {
                lua_numbertointeger($TMPNAME2, &$TMPNAME3);
            }
        ]], {
            CSTAT = cstat,
            CEXP = cexp,
            CTMP1 = ctmp1,
            CTMP2 = ctmp2,
            CTMP3 = ctmp3,
            TMPNAME1 = tmpname1,
            TMPNAME2 = tmpname2,
            TMPNAME3 = tmpname3,
            LINE = c_integer_literal(node.loc.line)
        })
        return cfloor, tmpname3
    elseif tag == "Ast.ExpCast" and node.target._tag == "Type.String" then
        local cvt
        local cstats, cexp = codeexp(ctx, node.exp)
        if node.exp._type._tag == "Type.Integer" then
            cvt = render("_integer2str(L, $EXP)", { EXP = cexp })
        elseif node.exp._type._tag == "Type.Float" then
            cvt = render("_float2str(L, $EXP)", { EXP = cexp })
        else
            error("invalid node type for coercion to string: " .. types.tostring(node.exp._type))
        end
        if target then
            return cstats, cvt
        else
            local ctmp, tmpname, tmpslot = newtmp(ctx, types.String(), true)
            local code = render([[
                $CTMP
                $TMPNAME = $CVT;
                setsvalue(L, $TMPSLOT, $TMPNAME);
            ]], {
                CTMP = ctmp,
                TMPNAME = tmpname,
                CVT = cvt,
                TMPSLOT = tmpslot,
            })
            return code, tmpname
        end
    elseif tag == "Ast.ExpCast" and node._type._tag == "Type.Pointer" then
        local cstats, cexp = codeexp(ctx, node.exp)
        local fstats, fexp = nativetoforeignexp(ctx, node.exp, cexp)
        return cstats .. fstats, foreigncast(fexp, node._type)
    elseif tag == "Ast.ExpConcat" then
        local strs, copies = {}, {}
        local ctmp, tmpname, tmpslot = newtmp(ctx, types.String(), true)
        for i, exp in ipairs(node.exps) do
            local cstat, cexp = codeexp(ctx, exp)
            local strvar = string.format('_str%d', i)
            local lenvar = string.format('_len%d', i)
            table.insert(strs, render([[
                $CSTAT
                TString *$STRVAR = $CEXP;
                size_t $LENVAR = tsslen($STRVAR);
                _len += $LENVAR;
            ]], {
                CSTAT = cstat,
                CEXP = cexp,
                STRVAR = strvar,
                LENVAR = lenvar,
            }))
            table.insert(copies, render([[
                memcpy(_buff + _tl, getstr($STRVAR), $LENVAR * sizeof(char));
                _tl += $LENVAR;
            ]], {
                STRVAR = strvar,
                LENVAR = lenvar,
            }))
        end
        local code = render([[
          $CTMP
          {
              size_t _len = 0;
              size_t _tl = 0;
              $STRS
              if(_len <= LUAI_MAXSHORTLEN) {
                  char _buff[LUAI_MAXSHORTLEN];
                  $COPIES
                  $TMPNAME = luaS_newlstr(L, _buff, _len);
              } else {
                  $TMPNAME = luaS_createlngstrobj(L, _len);
                  char *_buff = getstr($TMPNAME);
                  $COPIES
              }
          }
          setsvalue(L, $TMPSLOT, $TMPNAME);
        ]], {
            CTMP = ctmp,
            STRS = table.concat(strs, "\n"),
            COPIES = table.concat(copies, "\n"),
            TMPNAME = tmpname,
            TMPSLOT = tmpslot,
        })
        return code, tmpname
    elseif tag == "Ast.ExpAdjust" then
        return codeexp(ctx, node.exp, iscondition, target)
    else
        error("invalid node tag " .. tag)
    end
end

function codeforeignexp(ctx, node)
    local tag = node._tag
    if tag == "Ast.ExpCall"
           and node.exp.var._tag == "Ast.VarDot"
           and node.exp.var.exp._type._tag == "Type.ForeignModule" then
        return codeforeigncall(ctx, node)
    elseif tag == "Ast.VarDot" and node.exp._type._tag == "Type.ForeignModule" then
        return codeforeignvar(ctx, node)
    elseif tag == "Ast.ExpString" then
        return "", c_string_literal(node.value)
    elseif tag == "Ast.ExpCast" and node.target._tag == "Type.String" then
        local cstats, cexp = codeforeignexp(ctx, node.exp)
        return cstats, foreigncast(cexp, node._type)
    else
        local cstat, cexp = codeexp(ctx, node)
        local fstat, fexp = nativetoforeignexp(ctx, node, cexp)
        return cstat .. fstat, fexp
    end
end

-- Generate Lua entry point `lua_name` for a Titan
-- function or method with name `titan_name` and Titan
-- entry point `titan_entry`, type `typ`, and declared on location `loc`
local function genluaentry(tlcontext, titan_name, titan_entry, typ, loc, lua_name)
    -- generate Lua entry point
    local stats = {}
    local pnames = { "L" }
    local params
    if typ._tag == "Type.Function" or typ._tag == "Type.StaticMethod" then
        params = typ.params
    else
        assert(typ._tag == "Type.Method")
        params = { types.Nominal(typ.fqtn), table.unpack(typ.params) }
    end
    for i, param in ipairs(params) do
        local pname = "_param_" .. tostring(i)
        local slot = "(func+ " .. i .. ")"
        table.insert(pnames, pname)
        table.insert(stats, ctype(param) .. " " .. pname .. " = " .. initval(param) .. ";")
        table.insert(stats, checkandget(tlcontext, param, pname, slot, loc))
    end
    for i = 2, #typ.rettypes do
        local ptype = typ.rettypes[i]
        local pname = "_outparam_" .. i
        table.insert(pnames, "&" .. pname)
        table.insert(stats, ctype(ptype) .. " " .. pname .. " = " .. initval(ptype) .. ";")
    end
    local rettype = typ.rettypes[1]
    table.insert(stats, render([[
        lua_checkstack(L, $NRET);
        TValue *_firstret = L->top;
        L->top += $NRET;
        $TYPE res = $NAME($PARAMS);
        $SETSLOT;
        _firstret++;
    ]], {
        TYPE = ctype(rettype),
        NAME = titan_entry,
        PARAMS = table.concat(pnames, ", "),
        SETSLOT = setwrapped(rettype, "_firstret", "res"),
        NRET = #typ.rettypes
    }))
    for i = 2, #typ.rettypes do
        table.insert(stats, render([[
            $SETSLOT;
            _firstret++;
        ]], {
            SETSLOT = setwrapped(typ.rettypes[i], "_firstret", "_outparam_" .. i)
        }))
    end
    table.insert(stats, render([[
        return $NRET;
    ]], {
        NRET = #typ.rettypes
    }))
    return render([[
    static int $LUANAME(lua_State *L) {
        TValue *func = L->ci->func;
        if((L->top - func - 1) != $EXPECTED) {
            luaL_error(L, "calling Titan function %s with %d arguments, but expected %d", $NAME, L->top - func - 1, $EXPECTED);
        }
        $BODY
    }]], {
        LUANAME = lua_name,
        EXPECTED = c_integer_literal(#params),
        NAME = c_string_literal(titan_name),
        BODY = table.concat(stats, "\n"),
    })
end

-- Titan calling convention:
--     first parameter is a lua_State*, other parameters
--     get the other arguments, with each being its actual
--     native type. Garbage-collectable arguments also need
--     to be pushed to the Lua stack by the *caller*. The
--     function returns its first return value directly.
--     Other return values go in out parameters, following
--     the in parameters. Gc-able return values *do not*
--     need to be pushed by the callee, as it is assumed
--     that the caller is going to push them if needed and
--     just returning from a function does not call GC
local function codefuncdec(tlcontext, node)
    local ctx = newcontext(tlcontext)
    local stats = {}
    local rettype = node._type.rettypes[1]
    local fname, luaname
    if node._tag == "Ast.TopLevelFunc" then
        fname = func_name(tlcontext.module, node.name)
        luaname = func_luaname(tlcontext.module, node.name)
    elseif node._tag == "Ast.TopLevelStatic" then
        fname = static_name(node._type)
        luaname = static_luaname(node._type)
    else
        assert(node._tag == "Ast.TopLevelMethod")
        fname = method_name(node._type)
        luaname = method_luaname(node._type)
    end
    local cparams = { "lua_State *L" }
    if node._tag == "Ast.TopLevelMethod" then
        node._self._cvar = "__self__"
        if node._self._assigned then
            node._self._slot = "__selfslot__"
            table.insert(stats, newslot(ctx, node._self._slot))
        end
        table.insert(cparams, ctype(node._self._type) .. " " .. node._self._cvar)
    end
    for i, param in ipairs(node.params) do
        param._cvar = "_param_" .. param.name
        if types.is_gc(param._type) and param._assigned then
            param._slot = "_paramslot_" .. param.name
            table.insert(stats, newslot(ctx, param._slot))
        end
        table.insert(cparams, ctype(param._type) .. " " .. param._cvar)
    end
    for i = 2, #node._type.rettypes do
        local outtype = node._type.rettypes[i]
        table.insert(cparams, ctype(outtype) .. " *" .. "_outparam_" .. i)
    end
    local body = codestat(ctx, node.block)
    local nslots = ctx.nslots
    if nslots > 0 then
        table.insert(stats, 1, render([[
        luaC_checkGC(L);
        /* function preamble: reserve needed stack space */
        if (L->stack_last - L->top > $NSLOTS) {
            if (L->ci->top < L->top + $NSLOTS) L->ci->top = L->top + $NSLOTS;
        } else {
            lua_checkstack(L, $NSLOTS);
        }
        TValue *_base = L->top;
        L->top += $NSLOTS;
        for(TValue *_s = L->top - 1; _base <= _s; _s--) {
            setnilvalue(_s);
        }
        ]], {
            NSLOTS = c_integer_literal(nslots),
        }))
    end
    table.insert(stats, body)
    if rettype._tag == "Type.Nil" then
        if nslots > 0 then
            table.insert(stats, [[
            L->top = _base;
            return 0;]])
        else
            table.insert(stats, "        return 0;")
        end
    end
    node._body = render([[
    $ISLOCAL $RETTYPE $NAME($PARAMS) {
        $BODY
    }]], {
        ISLOCAL = node.islocal and "static" or "",
        RETTYPE = ctype(rettype),
        NAME = fname,
        PARAMS = table.concat(cparams, ", "),
        BODY = table.concat(stats, "\n")
    })
    node._sig = render([[
        $ISLOCAL $RETTYPE $NAME($PARAMS);
    ]], {
        ISLOCAL = node.islocal and "static" or "",
        RETTYPE = ctype(rettype),
        NAME = fname,
        PARAMS = table.concat(cparams, ", ")
    })
    node._luabody = genluaentry(tlcontext, node.name, fname, node._type, node.loc, luaname)
end

local function codevardec(tlctx, ctx, node)
    local cstats, cexp = codeexp(ctx, node.value)
    if node.islocal then
        node._cvar = "_global_" .. node.decl.name
        node._cdecl = "static " .. ctype(node._type) .. " " .. node._cvar .. ";"
        node._init = render([[
            $CSTATS
            $CVAR = $CEXP;
        ]], {
            CSTATS = cstats,
            CVAR = node._cvar,
            CEXP = cexp,
        })
        if types.is_gc(node._type) then
            node._slot = "_globalslot_" .. node.decl.name
            node._cdecl = "static TValue *" .. node._slot .. ";\n" ..
                node._cdecl
            node._init = render([[
                $INIT
                $SET;
            ]], {
                INIT = node._init,
                SET = setslot(node._type, node._slot, node._cvar)
            })
        end
    else
        node._slot = var_name(tlctx.module, node.decl.name)
        node._cdecl = "TValue *" .. node._slot .. ";"
        node._init = render([[
            $CSTATS
            $SET;
        ]], {
            CSTATS = cstats,
            SET = setslot(node._type, node._slot, cexp)
        })
    end
end

local preamble = [[
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "luaconf.h"
#include "lauxlib.h"
#include "lgc.h"
#include "ltable.h"
#include "lfunc.h"
#include "lstring.h"
#include "lvm.h"
#include "lobject.h"

#include "titan.h"

$FOREIGNIMPORTS

#ifdef __clang__
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wparentheses-equality"
#endif

$INCLUDES

$SIGS

]]

local postamble = [[
int $LUAOPEN_NAME(lua_State *L) {
    $INITNAME(L);
    lua_newtable(L);
    $FUNCS
    luaL_setmetatable(L, $MODNAMESTR);
    return 1;
}
]]

local init = [[
void $INITNAME(lua_State *L) {
    if(!_initialized) {
        _initialized = 1;
        $INITMODULES
        $INITRECORDS
        $INITVARS
    }
}
]]

local modtypes = [[
int $TYPESNAME(lua_State* L) {
    lua_pushliteral(L, $TYPES);
    return 1;
}
]]

local function prefix(initmods, mprefixes, modname)
    if mprefixes[modname] then return mprefixes[modname] end
    local mprefix = mangle_qn(modname) .. "_"
    local file = modname:gsub("[.]", "/") .. ".so"
    mprefixes[modname] = mprefix
    table.insert(initmods, render([[
        void *$HANDLE = loadlib(L, "$FILE");
        void (*$INIT)(lua_State *L) = cast_func(void (*)(lua_State*), loadsym(L, $HANDLE, "$INIT"));
        $INIT(L);
    ]], { HANDLE = mprefix .. "handle", INIT = mprefix .. "init", FILE = file}));
    return mprefix
end

local function init_data_from_other_modules(tlcontext, includes, initmods, mprefixes, is_dynamic)
    -- When linking dynamically, we need to make 'static' variables and load the values
    -- from the other module's dynamic library
    -- When linking statically, we only need to make 'extern' forward declarations.
    local storage_class = is_dynamic and "static" or "extern"

    for name, var in pairs(tlcontext.variables) do
        table.insert(includes, storage_class .. " TValue *" .. var.slot .. ";")
        if is_dynamic then
            table.insert(initmods, render([[
                $SLOT = *((TValue**)(loadsym(L, $HANDLE, "$SLOT")));
            ]], { SLOT = var.slot, HANDLE = mprefixes[var.module] .. "handle" }))
        end
    end

    for name, func in pairs(tlcontext.functions) do
        local fname = func.mangled
        table.insert(includes, storage_class .. " " .. function_sig(fname, func.type, is_dynamic) .. ";")
        if is_dynamic then
            local mprefix = prefix(initmods, mprefixes, func.module)
            table.insert(initmods, render([[
                $NAME = cast_func($TYPE, loadsym(L, $HANDLE, "$NAME"));
            ]], { NAME = fname, TYPE = function_sig("", func.type, true), HANDLE = mprefix .. "handle" }))
        end
    end

    for name, tag in pairs(tlcontext.tags) do
        table.insert(includes, storage_class .. " ptrdiff_t " .. tag.mangled .. ";")
        if is_dynamic then
            local mprefix = prefix(initmods, mprefixes, tag.module)
            table.insert(initmods, render([[
                $TAG = ((ptrdiff_t)(loadsym(L, $HANDLE, "$TAG")));
            ]], { TAG = tag.mangled, HANDLE = mprefix .. "handle" }))
        end
    end

    for name, meta in pairs(tlcontext.metatables) do
        table.insert(includes, storage_class .. " Table *" .. meta.mangled .. ";")
        if is_dynamic then
            local mprefix = prefix(initmods, mprefixes, meta.module)
            table.insert(initmods, render([[
                $META = *((Table**)(loadsym(L, $HANDLE, "$META")));
            ]], { META = meta.mangled, HANDLE = mprefix .. "handle" }))
        end
    end
end

function coder.generate(modname, ast, is_dynamic)
    local tlcontext = {
        module = modname,
        prefix = mangle_qn(modname) .. "_",
        variables = {},
        metatables = {},
        tags = {},
        functions = {}
    }

    local funcs = {}
    local initvars = {}
    local varslots = {}
    local gvars = {}
    local includes = {}
    local sigs = {}
    local foreignimports = {}
    local initmods = {}
    local mprefixes = {}
    local initrecs = {}

    local initctx = newcontext(tlcontext)

    for _, node in pairs(ast) do
        if not node._ignore then
            local tag = node._tag
            if tag == "Ast.TopLevelImport" then
                if is_dynamic then
                    prefix(initmods, mprefixes, node.modname)
                end
                for name, member in pairs(node._type.members) do
                    if not member._slot and member.type._tag ~= "Type.Function"
                      and member.type._tag ~= "Type.Record" then
                        member._slot = var_name(member.modname, name)
                    end
                end
            elseif tag == "Ast.TopLevelForeignImport" then
                local include
                if node.headername:match("^/") then
                    include = [[#include "$HEADERNAME"]]
                else
                    include = [[#include <$HEADERNAME>]]
                end
                table.insert(foreignimports, render(include, {
                    HEADERNAME = node.headername
                }))
            else
                -- ignore functions and variables in this pass
            end
        end
    end

    local code = {}

    -- has this module already been initialized?
    table.insert(code, "static int _initialized = 0;")

    for _, node in pairs(ast) do
        if not node._ignore then
            local tag = node._tag
            if tag == "Ast.TopLevelVar" then
                codevardec(tlcontext, initctx, node)
                table.insert(code, node._cdecl)
                table.insert(initvars, node._init)
                table.insert(varslots, node._slot)
                if not node.islocal then
                    table.insert(gvars, node)
                end
            elseif tag == "Ast.TopLevelRecord" then
                table.insert(code, "int " .. tagname(node._type.name) .. ";")
                table.insert(code, "Table *" .. metaname(node._type.name) .. ";")
                local fieldmap, getswitch, setswitch = {}, {}, {}
                for i, field in ipairs(node._type.fields) do
                    table.insert(fieldmap, render([[
                        lua_pushliteral(L, $NAME);
                        lua_pushinteger(L, $INDEX);
                        lua_rawset(L, -3);
                    ]], {
                        NAME = c_string_literal(field.name),
                        INDEX = c_integer_literal(field.index)
                    }))
                    table.insert(getswitch, render([[
                        case $INDEX: {
                            $SETWRAPPED
                            break;
                        }
                    ]], {
                        INDEX = c_integer_literal(field.index),
                        SETWRAPPED = field.type._tag == "Type.Nominal" and
                            "setobj2t(L, L->top-1, &((clCvalue(_slot))->upvalue[0]));" or
                            "setobj2t(L, L->top-1, _slot);"
                    }))
                    table.insert(setswitch, render([[
                        case $INDEX: {
                            $CHECKANDSET;
                            break;
                        }
                    ]], {
                        INDEX = c_integer_literal(field.index),
                        CHECKANDSET = checkandset(tlcontext, field.type, "_dst", "_src", node.loc)
                    }))
                end
                for name, method in pairs(node._type.methods) do
                    table.insert(fieldmap, render([[
                        lua_pushliteral(L, $NAME);
                        lua_pushcclosure(L, $METHOD, 0);
                        lua_rawset(L, -3);
                    ]], {
                        NAME = c_string_literal(name),
                        METHOD = method_luaname(method)
                    }))
                end
                if #node._type.fields > 0 then
                    table.insert(code, render([[
                        static int __index_$RECNAME(lua_State *L) {
                            TValue *_recslot = L->ci->func + 1;
                            CClosure *_rec = NULL;
                            $CHECKANDGET;
                            TValue *_fieldslot = L->ci->func + 2;
                            CClosure *_func = clCvalue(L->ci->func);
                            Table *_fieldmap = hvalue(&(_func->upvalue[0]));
                            const TValue *_fieldidx = luaH_get(_fieldmap, _fieldslot);
                            if(TITAN_LIKELY(_fieldidx != luaO_nilobject)) {
                                if(ttislcf(_fieldidx)) {
                                    setobj2t(L, L->top-1, _fieldidx);
                                } else {
                                    TValue *_slot = &_rec->upvalue[ivalue(_fieldidx)];
                                    switch(ivalue(_fieldidx)) {
                                        $GETCASES
                                    }
                                }
                                return 1;
                            } else {
                                return luaL_error(L, "%s:%d:%d: field '%s' not found in record '%s'", $FILE, $LINE, $COL, lua_tostring(L, 2), $FQTN);
                            }
                        }
                        static int __newindex_$RECNAME(lua_State *L) {
                            TValue *_recslot = L->ci->func + 1;
                            CClosure *_rec = NULL;
                            $CHECKANDGET;
                            TValue *_fieldslot = L->ci->func + 2;
                            CClosure *_func = clCvalue(L->ci->func);
                            Table *_fieldmap = hvalue(&(_func->upvalue[0]));
                            const TValue *_fieldidx = luaH_get(_fieldmap, _fieldslot);
                            if(TITAN_LIKELY(_fieldidx != luaO_nilobject && ttisinteger(_fieldidx))) {
                                TValue* _dst = &(_rec->upvalue[ivalue(_fieldidx)]);
                                TValue* _src = L->ci->func + 3;
                                switch(ivalue(_fieldidx)) {
                                    $SETCASES
                                }
                                return 0;
                            } else {
                                return luaL_error(L, "%s:%d:%d: field '%s' not found in record '%s'", $FILE, $LINE, $COL, lua_tostring(L, 2), $FQTN);
                            }
                        }
                    ]], {
                        RECNAME = mangle_qn(node._type.name),
                        CHECKANDGET = checkandget(tlcontext, types.Nominal(node._type.name), "_rec", "_recslot", node.loc),
                        FQTN = c_string_literal(node._type.name),
                        FILE = c_string_literal(node.loc.filename),
                        LINE = c_integer_literal(node.loc.line),
                        COL = c_integer_literal(node.loc.col),
                        GETCASES = table.concat(getswitch, "\n"),
                        SETCASES = table.concat(setswitch, "\n")
                    }))
                else
                    table.insert(code, render([[
                        static int __index_$RECNAME(lua_State *L) {
                            return luaL_error(L, "%s:%d:%d: field %s not found in record %s", $FILE, $LINE, $COL, lua_tostring(L, 2), $FQTN);
                        }
                        static int __newindex_$RECNAME(lua_State *L) {
                            return luaL_error(L, "%s:%d:%d: field %s not found in record %s", $FILE, $LINE, $COL, lua_tostring(L, 2), $FQTN);
                        }
                    ]], {
                        RECNAME = mangle_qn(node._type.name),
                        FQTN = c_string_literal(node._type.name),
                        FILE = c_string_literal(node.loc.filename),
                        LINE = c_integer_literal(node.loc.line),
                        COL = c_integer_literal(node.loc.col),
                    }))
                end
                table.insert(initrecs, render([[
                    luaL_newmetatable(L, "Titan record $TYPE"); /* push metatable */
                    $META = hvalue(L->top);
                    lua_pushliteral(L, "__metatable");
                    lua_pushliteral(L, "Titan record $TYPE");
                    lua_rawset(L, -3);
                    lua_newtable(L);
                    $FIELDMAP
                    lua_pushliteral(L, "__index");
                    lua_pushvalue(L, -2);
                    lua_pushcclosure(L, __index_$RECNAME, 1);
                    lua_rawset(L, -4);
                    lua_pushliteral(L, "__newindex");
                    lua_pushvalue(L, -2);
                    lua_pushcclosure(L, __newindex_$RECNAME, 1);
                    lua_rawset(L, -4);
                    L->top -= 2;
                    lua_pushlightuserdata(L, &$TAG);
                    lua_pushstring(L, "$TYPE");
                    lua_rawset(L, LUA_REGISTRYINDEX);
                ]], {
                    RECNAME = mangle_qn(node._type.name),
                    TAG = tagname(node._type.name),
                    TYPE = node._type.name,
                    META = metaname(node._type.name),
                    FIELDMAP = table.concat(fieldmap, "\n")
                }))
            else
                -- ignore everything else in this pass
            end
        end
    end

    for _, node in pairs(ast) do
        if not node._ignore then
            local tag = node._tag
            if tag == "Ast.TopLevelFunc" or tag == "Ast.TopLevelMethod" or tag == "Ast.TopLevelStatic" then
                codefuncdec(tlcontext, node)
                table.insert(sigs, node._sig)
                table.insert(code, node._body)
                if tag == "Ast.TopLevelFunc" and not node.islocal then
                    table.insert(code, node._luabody)
                    table.insert(funcs, render([[
                        lua_pushcfunction(L, $LUANAME);
                        lua_setfield(L, -2, $NAMESTR);
                    ]], {
                        LUANAME = func_luaname(tlcontext.module, node.name),
                        NAMESTR = c_string_literal(node.name),
                    }))
                elseif tag == "Ast.TopLevelMethod" then
                    table.insert(code, node._luabody)
                elseif tag == "Ast.TopLevelStatic" then
                    table.insert(code, node._luabody)
                    table.insert(funcs, render([[
                        lua_getfield(L, -1, $RECNAME);
                        lua_pushcfunction(L, $LUANAME);
                        lua_setfield(L, -2, $NAMESTR);
                        lua_pop(L, 1);
                    ]], {
                        RECNAME = c_string_literal(node.class),
                        LUANAME = static_luaname(node._type),
                        NAMESTR = c_string_literal(node.name),
                    }))
                end
            elseif tag == "Ast.TopLevelRecord" then
                table.insert(funcs, render([[
                    lua_newtable(L);
                    lua_setfield(L, -2, $NAMESTR);
                ]], {
                    NAMESTR = c_string_literal(node.name),
                }))
            else
                -- ignore other nodes in second pass
            end
        end
    end

    if initctx.nslots + #varslots > 0 then
        local switch_get, switch_set = {}, {}

        for i, var in ipairs(gvars) do
            if var._type._tag == "Type.Nominal" then
                table.insert(switch_get, render([[
                    case $I: setobj2t(L, L->top-1, &(clCvalue($SLOT)->upvalue[0])); break;
                ]], {
                    I = c_integer_literal(i),
                    SLOT = var._slot
                }))
            else
                table.insert(switch_get, render([[
                    case $I: setobj2t(L, L->top-1, $SLOT); break;
                ]], {
                    I = c_integer_literal(i),
                    SLOT = var._slot
                }))
            end
            table.insert(switch_set, render([[
                case $I: {
                    lua_pushvalue(L, 3);
                    $SETSLOT;
                    break;
                }
            ]], {
                I = c_integer_literal(i),
                SETSLOT = checkandset(tlcontext, var._type, var._slot, "L->top-1", var.loc)
            }))
        end

        table.insert(code, render([[
            static int __index(lua_State *L) {
                lua_pushvalue(L, 2);
                lua_rawget(L, lua_upvalueindex(1));
                if(lua_isnil(L, -1)) {
                    return luaL_error(L,
                        "global variable '%s' does not exist in Titan module '%s'",
                        lua_tostring(L, 2), $MODSTR);
                }
                switch(lua_tointeger(L, -1)) {
                    $SWITCH_GET
                }
                return 1;
            }

            static int __newindex(lua_State *L) {
                lua_pushvalue(L, 2);
                lua_rawget(L, lua_upvalueindex(1));
                if(lua_isnil(L, -1)) {
                    return luaL_error(L,
                        "global variable '%s' does not exist in Titan module '%s'",
                        lua_tostring(L, 2), $MODSTR);
                }
                switch(lua_tointeger(L, -1)) {
                    $SWITCH_SET
                }
                return 1;
            }
         ]], {
             MODSTR = c_string_literal(modname),
             SWITCH_GET = table.concat(switch_get, "\n"),
             SWITCH_SET = table.concat(switch_set, "\n"),
         }))

        local nslots = initctx.nslots + #varslots + 1

        table.insert(initvars, 1, render([[
            luaL_newmetatable(L, $MODNAMESTR); /* push metatable */
            int _meta = lua_gettop(L);
            TValue *_base = L->top;
            /* protect it */
            lua_pushliteral(L, $MODNAMESTR);
            lua_setfield(L, -2, "__metatable");
            /* reserve needed stack space */
            if (L->stack_last - L->top > $NSLOTS) {
                if (L->ci->top < L->top + $NSLOTS) L->ci->top = L->top + $NSLOTS;
            } else {
                lua_checkstack(L, $NSLOTS);
            }
            L->top += $NSLOTS;
            for(TValue *_s = L->top - 1; _base <= _s; _s--) {
                setnilvalue(_s);
            }
            Table *_map = luaH_new(L);
            sethvalue(L, L->top-$VARSLOTS, _map);
            lua_pushcclosure(L, __index, $VARSLOTS);
            TValue *_upvals = clCvalue(L->top-1)->upvalue;
            lua_setfield(L, _meta, "__index");
            sethvalue(L, L->top, _map);
            L->top++;
            lua_pushcclosure(L, __newindex, 1);
            lua_setfield(L, _meta, "__newindex");
            L->top++;
            sethvalue(L, L->top-1, _map);
        ]], {
            MODNAMESTR = c_string_literal("titan module "..modname),
            NSLOTS = c_integer_literal(nslots),
            VARSLOTS = c_integer_literal(#varslots+1),
        }))
        for i, slot in ipairs(varslots) do
            table.insert(initvars, i+1, render([[
                $SLOT = &_upvals[$I];
            ]], {
                SLOT = slot,
                I = c_integer_literal(i),
            }))
        end
        for i, var in ipairs(gvars) do
            table.insert(initvars, 2, render([[
                lua_pushinteger(L, $I);
                lua_setfield(L, -2, $NAME);
            ]], {
                I = c_integer_literal(i),
                NAME = c_string_literal(var.decl.name)
            }))
        end
        table.insert(initvars, [[
        L->top = _base-1;
        ]])
    end

    table.insert(code, render(modtypes, {
        TYPESNAME = tlcontext.prefix .. "types",
        TYPES = string.format("%q", types.serialize(ast._type))
    }))

    init_data_from_other_modules(tlcontext, includes, initmods, mprefixes, is_dynamic)

    table.insert(code, render(init, {
        INITNAME = tlcontext.prefix .. 'init',
        INITMODULES = table.concat(initmods, "\n"),
        INITVARS = table.concat(initvars, "\n"),
        INITRECORDS = table.concat(initrecs, "\n")
    }))

    table.insert(code, render(postamble, {
        LUAOPEN_NAME = 'luaopen_' .. mangle_qn(modname),
        INITNAME = tlcontext.prefix .. 'init',
        FUNCS = table.concat(funcs, "\n"),
        MODNAMESTR = c_string_literal("titan module "..modname),
    }))

    local preamble = render(preamble, {
        INCLUDES = table.concat(includes, "\n"),
        SIGS = table.concat(sigs, "\n"),
        FOREIGNIMPORTS = table.concat(foreignimports, "\n"),
    })

    return preamble .. "\n\n" .. table.concat(code, "\n\n")
end

return coder
