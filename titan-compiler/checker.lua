local checker = {}

local location = require "titan-compiler.location"
local symtab = require "titan-compiler.symtab"
local types = require "titan-compiler.types"
local ast = require "titan-compiler.ast"
local util = require "titan-compiler.util"
local foreigntypes = require "titan-compiler.foreigntypes"
local cdriver = require("c-parser.cdriver")


-- The typechecker works in two passes, the first one just
-- collects type information for top-level functions and variables
-- (and detects duplicate definitions in the top level), while the
-- second pass does the actual typechecking. All typechecked nodes
-- that have a type get a "_type" field with the type. The types
-- themselves are in "types.lua".
local typefromnode

local checkdecl
local checkstat
local checkexp
local checkvar

function checker.typeerror(errors, loc, fmt, ...)
    local errmsg = location.format_error(loc, "type error: "..fmt, ...)
    table.insert(errors, errmsg)
end

-- Checks if a nominal type 'type' is valid
local function checktype(type, loc, errors)
    if type._tag == "Type.Nominal" and
        not types.registry[type.fqtn] then
        checker.typeerror(errors, loc,
            "invalid type '%s' in type declaration", type.fqtn)
    end
end

-- Checks if two types are the same, and logs an error message otherwise
--   term: string describing what is being compared
--   expected: type that is expected
--   found: type that was actually present
--   errors: list of compile-time errors
--   loc: location of the term that is being compared
local function checkmatch(term, expected, found, errors, loc)
    if types.coerceable(found, expected) or not types.compatible(expected, found) then
        local msg = "types in %s do not match, expected %s but found %s"
        msg = string.format(msg, term, types.tostring(expected), types.tostring(found))
        checker.typeerror(errors, loc, msg)
    end
end

-- Converts an AST type declaration into a typechecker type
--   node: AST node
--   errors: list of compile-time errors
--   returns a type (from types.lua)
typefromnode = util.make_visitor({
    ["Ast.TypeNil"] = function(node, st, errors)
        return types.Nil()
    end,

    ["Ast.TypeBoolean"] = function(node, st, errors)
        return types.Boolean()
    end,

    ["Ast.TypeInteger"] = function(node, st, errors)
        return types.Integer()
    end,

    ["Ast.TypeFloat"] = function(node, st, errors)
        return types.Float()
    end,

    ["Ast.TypeString"] = function(node, st, errors)
        return types.String()
    end,

    ["Ast.TypeValue"] = function(node, st, errors)
        return types.Value()
    end,

    ["Ast.TypeName"] = function(node, st, errors)
        return types.Nominal(st.modname .. "." .. node.name)
    end,

    ["Ast.TypeQualName"] = function(node, st, errors)
        local mod = st:find_symbol(node.module)
        if mod and mod._type._tag == "Type.Module" then
            local fqtn = mod._type.name .. "." .. node.name
            return types.Nominal(fqtn)
        else
            checker.typeerror(errors, node.loc, "module '%s' referenced by type not found", node.module)
        end
        return types.Invalid()
    end,

    ["Ast.TypeArray"] = function(node, st, errors)
        return types.Array(typefromnode(node.subtype, st, errors))
    end,

    ["Ast.TypeMap"] = function(node, st, errors)
        return types.Map(typefromnode(node.keystype, st, errors),
                         typefromnode(node.valuestype, st, errors))
    end,

    ["Ast.TypeFunction"] = function(node, st, errors)
        if #node.argtypes ~= 1 then
            error("functions with 0 or 2+ return values are not yet implemented")
        end
        local ptypes = {}
        for _, ptype in ipairs(node.argtypes) do
            table.insert(ptypes, typefromnode(ptype, st, errors))
        end
        local rettypes = {}
        for _, rettype in ipairs(node.rettypes) do
            table.insert(rettypes, typefromnode(rettype, st, errors))
        end
        return types.Function(ptypes, rettypes, false)
    end,
})

-- tries to coerce node to target type
--    node: expression node
--    target: target type
--    returns node wrapped in a coercion, or original node
local function trycoerce(node, target, errors)
    if types.coerceable(node._type, target) then
        local n = ast.ExpCast(node.loc, node, target)
        n._type = target
        return n
    else
        return node
    end
end

local function trytostr(node)
    local source = node._type
    if source._tag == "Type.Integer" or
       source._tag == "Type.Float" then
        local n = ast.ExpCast(node.loc, node, types.String())
        n._type = types.String()
        return n
    else
        return node
    end
end

--
-- Decl
--

checkdecl = function(node, st, errors)
    node._type = node._type or typefromnode(node.type, st, errors)
    checktype(node._type, node.loc, errors)
end

local function declare(node, st)
    st:add_symbol(node.name, node)
end

--
-- Stat
--

-- Typechecks a repeat/until statement
--   node: StatRepeat AST node
--   st: symbol table
--   errors: list of compile-time errors
--   returns whether statement always returns from its function (always false for repeat/until)
local function checkrepeat(node, st, errors)
    for _, stat in ipairs(node.block.stats) do
        checkstat(stat, st, errors)
    end
    checkexp(node.condition, st, errors, types.Boolean())
    return false
end

-- Typechecks a for loop statement
--   node: StatFor AST node
--   st: symbol table
--   errors: list of compile-time errors
--   returns whether statement always returns from its function (always false for 'for' loop)
local function checkfor(node, st, errors)
    checkexp(node.start, st, errors)
    checkexp(node.finish, st, errors)
    if node.inc then
        checkexp(node.inc, st, errors)
    end

    -- Add loop variable to symbol table only after checking expressions
    if not node.decl.type then
        node.decl._type = node.start._type
    end
    checkdecl(node.decl, st, errors)
    declare(node.decl, st)

    local loop_type_is_valid
    if node.decl._type._tag == "Type.Integer" then
        loop_type_is_valid = true
        if not node.inc then
            node.inc = ast.ExpInteger(node.finish.loc, 1)
            node.inc._type = types.Integer()
        end
    elseif node.decl._type._tag == "Type.Float" then
        loop_type_is_valid = true
        if not node.inc then
            node.inc = ast.ExpFloat(node.finish.loc, 1.0)
            node.inc._type = types.Float()
        end
    else
        loop_type_is_valid = false
        checker.typeerror(errors, node.decl.loc,
            "type of for control variable %s must be integer or float",
            node.decl.name)
    end

    if loop_type_is_valid then
        checkmatch("'for' start expression",
            node.decl._type, node.start._type, errors, node.start.loc)
        checkmatch("'for' finish expression",
            node.decl._type, node.finish._type, errors, node.finish.loc)
        checkmatch("'for' step expression",
            node.decl._type, node.inc._type, errors, node.inc.loc)
    end

    checkstat(node.block, st, errors)

    return false
end

-- Typechecks a block statement
--   node: StatBlock AST node
--   st: symbol table
--   errors: list of compile-time errors
--   returns whether the block always returns from the containing function
local function checkblock(node, st, errors)
    local ret = false
    for _, stat in ipairs(node.stats) do
        ret = ret or checkstat(stat, st, errors)
    end
    return ret
end

-- Typechecks a statement or declaration
--   node: A DeclDecl or Stat_* AST node
--   st: symbol table
--   errors: list of compile-time errors
--   returns whether statement always returns from its function (always false for repeat/until)
checkstat = util.make_visitor({
    ["Ast.StatDecl"] = function(node, st, errors)
        local nlastexp = #node.exps
        local lastexp = node.exps[nlastexp]
        for i = 1, #node.exps do
            local decl = node.decls[i]
            local exp = node.exps[i]
            if decl then
                if decl.type then
                    checkdecl(decl, st, errors)
                    checkexp(exp, st, errors, decl._type)
                else
                    checkexp(exp, st, errors)
                    decl._type = exp._type
                    checkdecl(decl, st, errors)
                end
                exp = trycoerce(exp, decl._type, errors)
                node.exps[i] = exp
                checkmatch("declaration of local variable " .. decl.name,
                    decl._type, exp._type, errors, decl.loc)
            end
        end
        if lastexp._types then -- multiple return values
            for i = 2, #lastexp._types do
                local decl = node.decls[nlastexp + i - 1]
                local exp = ast.ExpExtra(lastexp.loc, lastexp, i, lastexp._types[i])
                if decl then
                    if decl.type then
                        checkdecl(decl, st, errors)
                        exp = trycoerce(exp, decl._type, errors)
                    else
                        decl._type = exp._type
                    end
                    checkmatch("declaration of local variable " .. decl.name,
                        decl._type, exp._type, errors, decl.loc)
                end
                table.insert(node.exps, exp)
            end
        end
        if #node.decls ~= #node.exps then
            checker.typeerror(errors, node.loc, "left-hand side expects %d value(s) but right-hand side produces %d value(s)", #node.decls, #node.exps)
        end
        for _, decl in ipairs(node.decls) do
            declare(decl, st)
        end
    end,

    ["Ast.StatBlock"] = function(node, st, errors)
        return st:with_block(checkblock, node, st, errors)
    end,

    ["Ast.StatWhile"] = function(node, st, errors)
        checkexp(node.condition, st, errors, types.Boolean())
        st:with_block(checkstat, node.block, st, errors)
    end,

    ["Ast.StatRepeat"] = function(node, st, errors)
        st:with_block(checkrepeat, node, st, errors)
    end,

    ["Ast.StatFor"] = function(node, st, errors)
        st:with_block(checkfor, node, st, errors)
    end,

    ["Ast.StatAssign"] = function(node, st, errors)
        for _, var in ipairs(node.vars) do
            checkvar(var, st, errors)
        end
        for i, exp in ipairs(node.exps) do
            checkexp(exp, st, errors, node.vars[i] and node.vars[i]._type)
        end
        local nexps = #node.exps
        local lastexp = node.exps[nexps]
        if lastexp._types and #lastexp._types > 1 then
            for i = 2, #lastexp._types do
                table.insert(node.exps, ast.ExpExtra(lastexp.loc, lastexp, i, lastexp._types[i]))
            end
        end
        if #node.vars ~= #node.exps then
            checker.typeerror(errors, node.loc, "left-hand side expects %d value(s) but right-hand side produces %d value(s)", #node.vars, #node.exps)
        end
        for i, exp in ipairs(node.exps) do
            local var = node.vars[i]
            if var then
                local texp = var._type
                if texp._tag == "Type.Module" then
                    checker.typeerror(errors, var.loc, "trying to assign to a module")
                elseif texp._tag == "Type.Function" then
                    checker.typeerror(errors, var.loc, "trying to assign to a function")
                else
                    -- mark this declared variable as assigned to
                    if var._tag == "Ast.VarName" and var._decl then
                        var._decl._assigned = true
                    end
                    exp = trycoerce(exp, var._type, errors)
                    node.exps[i] = exp
                    if var._tag ~= "Ast.VarBracket" or exp._type._tag ~= "Type.Nil" then
                        checkmatch("assignment", var._type, exp._type, errors, var.loc)
                    end
                end
            end
        end
    end,

    ["Ast.StatCall"] = function(node, st, errors)
        checkexp(node.callexp, st, errors)
    end,

    ["Ast.StatReturn"] = function(node, st, errors)
        local ftype = st:find_symbol("$function")._type
        for i, exp in ipairs(node.exps) do
            checkexp(exp, st, errors, ftype.rettypes[i])
        end
        local lastexp = node.exps[#node.exps]
        if lastexp._types and #lastexp._types > 1 then
            for i = 2, #lastexp._types do
                table.insert(node.exps, ast.ExpExtra(lastexp.loc, lastexp, i, lastexp._types[i]))
            end
        end
        if #ftype.rettypes ~= #node.exps then
            checker.typeerror(errors, node.loc, "returned %d value(s) but function expected %d", #node.exps, #ftype.rettypes)
        end
        for i, tret in ipairs(ftype.rettypes) do
            local exp = node.exps[i]
            if exp then
                exp = trycoerce(exp, tret, errors)
                node.exps[i] = exp
                checkmatch("return", tret, exp._type, errors, exp.loc)
            end
        end
        return true
    end,

    ["Ast.StatIf"] = function(node, st, errors)
        local ret = true
        for _, thn in ipairs(node.thens) do
            checkexp(thn.condition, st, errors, types.Boolean())
            ret = checkstat(thn.block, st, errors) and ret
        end
        if node.elsestat then
            ret = checkstat(node.elsestat, st, errors) and ret
        else
            ret = false
        end
        return ret
    end,
})

--
-- Var
--

-- Typechecks an variable node
--   node: Var_* AST node
--   st: symbol table
--   errors: list of compile-time errors
--   context: expected type for this expression, if applicable
--   annotates the node with its type in a "_type" field
checkvar = util.make_visitor({
    ["Ast.VarName"] = function(node, st, errors, context)
        local decl = st:find_symbol(node.name)
        if not decl then
            checker.typeerror(errors, node.loc,
                "variable '%s' not declared", node.name)
            node._type = types.Invalid()
        else
            node._decl = decl
            node._type = decl._type
        end
    end,

    ["Ast.VarDot"] = function(node, st, errors)
        if node.exp._tag == "Ast.ExpVar" and
          (node.exp.var._tag == "Ast.VarName" or (node.exp.var._tag == "Ast.VarDot" and
              node.exp.var.exp._tag == "Ast.ExpVar" and
              node.exp.var.exp.var._tag == "Ast.VarName" and
              node.name == "new")) then
            checkvar(node.exp.var, st, errors)
            if node.exp.var._decl then node.exp.var._decl._used = true end
            node.exp._type = node.exp.var._type
        else
            checkexp(node.exp, st, errors)
        end
        local ltype = node.exp._type
        if ltype._tag == "Type.Module" or ltype._tag == "Type.ForeignModule" then
            local mod = ltype
            if not mod.members[node.name] then
                checker.typeerror(errors, node.loc,
                    "member '%s' not found inside module '%s'",
                    node.name, mod.name)
            else
                node._decl = mod.members[node.name]
                node._type = node._decl.type
            end
        elseif ltype._tag == "Type.Record" then
            local decl = ltype.functions[node.name]
            if decl then
                node._decl = decl
                node._type = types.Function(decl.params, decl.rettypes, false)
            else
                checker.typeerror(errors, node.loc,
                    "trying to access invalid record function '%s'", node.name)
            end
        elseif ltype._tag == "Type.Nominal" then
            local type = types.registry[ltype.fqtn]
            if not type then
                checker.typeerror(errors, node.loc,
                    "type '%s' not found", ltype.fqtn)
            elseif type._tag ~= "Type.Record" then
                checker.typeerror(errors, node.loc,
                    "trying to access field '%s' of type '%s' that is not a record but '%s'",
                    node.name, ltype.fqtn, type._tag)
            else
                for _, field in ipairs(type.fields) do
                    if field.name == node.name then
                        node._decl = field
                        node._type = field.type
                        break
                    end
                end
                if not node._type then
                    checker.typeerror(errors, node.loc,
                        "field '%s' not found in record '%s'",
                        node.name, ltype.fqtn)
                end
            end
        else
            checker.typeerror(errors, node.loc,
                "trying to access a member of value of type '%s'",
                types.tostring(ltype))
        end
        node._type = node._type or types.Invalid()
    end,

    ["Ast.VarBracket"] = function(node, st, errors, context)
        checkexp(node.exp1, st, errors) -- FIXME check this: checkexp(node.exp1, st, errors, context and types.Array(context))
        if node.exp1._type._tag == "Type.Array" then
            node._type = node.exp1._type.elem

            checkexp(node.exp2, st, errors, types.Integer())
            -- always try to coerce index to integer
            node.exp2 = trycoerce(node.exp2, types.Integer(), errors)
            checkmatch("array indexing", types.Integer(), node.exp2._type, errors, node.exp2.loc)

        elseif node.exp1._type._tag == "Type.Map" then
            node._type = node.exp1._type.values

            local keystype = node.exp1._type.keys
            checkexp(node.exp2, st, errors, keystype)
            checkmatch("map indexing", keystype, node.exp2._type, errors, node.exp2.loc)

        else
            checker.typeerror(errors, node.exp1.loc,
                "expression in indexing is not an array or map but %s",
                types.tostring(node.exp1._type))
            node._type = types.Invalid()
        end
    end,
})

--
-- Exp
--

-- Returns best guess of expression name
local function expname(node)
    if node._tag == "Ast.ExpVar" then
        if node.var._tag == "Ast.VarName" then
            return node.var.name
        elseif node.var._tag == "Ast.VarDot" then
            return expname(node.var.exp) .. "." .. node.var.name
        else
            return "(expression)"
        end
    else
        return "(expression)"
    end
end

-- Typechecks and argument list for a function or method call
local function checkargs(ftype, args, loc, fname, st, errors)
    local nparams = #ftype.params
    local nargs = #args
    local lastarg = args[nargs]
    for i = 1, nargs do
        local arg = args[i]
        local ptype = ftype.params[i]
        checkexp(arg, st, errors, ptype)
        ptype = ptype or arg._type
        args[i] = trycoerce(args[i], ptype, errors)
        local atype = args[i]._type
        checkmatch("argument " .. i .. " of call to " .. fname, ptype, atype, errors, arg.loc)
    end
    if lastarg and lastarg._types then
        for i = 2, #lastarg._types do
            local pidx = nargs + i - 1
            local ptype = ftype.params[pidx]
            local exp = ast.ExpExtra(lastarg.loc, lastarg, i, lastarg._types[i])
            ptype = ptype or exp._type
            exp = trycoerce(exp, ptype, errors)
            table.insert(args, exp)
            checkmatch("argument " .. pidx .. " of call to " .. fname, ptype, exp._type, errors, exp.loc)
        end
    end
    if not (#args == nparams or (ftype.vararg and #args > nparams)) then
        checker.typeerror(errors, loc,
            "%s called with %d arguments but expects %d.\n%s",
            fname, #args, nparams, types.tostring(ftype))
    end
end

-- Typechecks an expression
--   node: Exp_* AST node
--   st: symbol table
--   errors: list of compile-time errors
--   context: expected type for this expression, if applicable
--   annotates the node with its type in a "_type" field
checkexp = util.make_visitor({
    ["Ast.ExpNil"] = function(node)
        node._type = types.Nil()
    end,

    ["Ast.ExpBool"] = function(node)
        node._type = types.Boolean()
    end,

    ["Ast.ExpInteger"] = function(node)
        node._type = types.Integer()
    end,

    ["Ast.ExpFloat"] = function(node)
        node._type = types.Float()
    end,

    ["Ast.ExpString"] = function(node)
        node._type = types.String()
    end,

    ["Ast.ExpInitList"] = function(node, st, errors, context)
        local expected = context and context._tag
        if not expected then
            -- try to detect from contents
            local elem = node.fields[1]
            if elem then
                if elem.name and type(elem.name) == "table" then
                    expected = "Type.Map"
                elseif elem.name and type(elem.name) == "string" then
                    expected = "Type.Nominal"
                end
            end
            -- fall back to array if not detected otherwise
            if not expected then
                expected = "Type.Array"
            end
        end

        if expected == "Type.Array" then
            local econtext = context and context.elem
            local etypes = {}
            for _, field in ipairs(node.fields) do
                if field.name then
                    checker.typeerror(errors, field.loc,
                        "initializing field '%s' when expecting array", field.name)
                    checkexp(field.exp, st, errors)
                else
                    local exp = field.exp
                    checkexp(exp, st, errors, econtext)
                    table.insert(etypes, exp._type)
                end
            end

            -- adjust last entry if it's a function call that expands to multiple values
            -- e.g. { 1, 2, f() }
            local lastfield = node.fields[#node.fields]
            if lastfield and not lastfield.name and lastfield.exp._types and #lastfield.exp._types > 1 then
                for i = 2, #lastfield.exp._types do
                    table.insert(node.fields,
                        ast.Field(lastfield.loc, nil,
                            ast.ExpExtra(lastfield.loc, lastfield.exp,
                                i, lastfield.exp._types[i])))
                end
            end

            local etype = econtext or etypes[1] or types.Integer()
            node._type = types.Array(etype)
            for i, field in ipairs(node.fields) do
                if not field.name then
                    field.exp = trycoerce(field.exp, etype, errors)
                    local exp = field.exp
                    checkmatch("array initializer at position " .. i, etype,
                               exp._type, errors, exp.loc)
                end
            end
        elseif expected == "Type.Map" then
            local kcontext = context and context.keys
            local vcontext = context and context.values
            local ktypes = {}
            local vtypes = {}
            for _, field in ipairs(node.fields) do
                if type(field.name) == "string" then
                    checker.typeerror(errors, field.loc,
                        "initializing field '%s' when expecting map", field.name)
                    checkexp(field.exp, st, errors)
                else
                    local kexp = field.name
                    checkexp(kexp, st, errors, kcontext)
                    table.insert(ktypes, kexp._type)

                    local vexp = field.exp
                    checkexp(vexp, st, errors, vcontext)
                    table.insert(vtypes, vexp._type)
                end
            end

            local ktype = kcontext or ktypes[1]
            local vtype = vcontext or vtypes[1]
            node._type = types.Map(ktype, vtype)
            for i, field in ipairs(node.fields) do
                if not field.name then
                    field.name = trycoerce(field.name, ktype, errors)
                    local kexp = field.name
                    checkmatch("map key initializer at position " .. i, ktype,
                               kexp._type, errors, kexp.loc)

                    field.exp = trycoerce(field.exp, vtype, errors)
                    local vexp = field.exp
                    checkmatch("map value initializer at position " .. i, vtype,
                               vexp._type, errors, vexp.loc)
                end
            end
        elseif expected == "Type.Nominal" then
            if not types.registry[context.fqtn] then
                checker.typeerror(errors, node.loc,
                    "record type '%s' in context of record constructor does not exist",
                    types.tostring(context))
                for _, field in ipairs(node.fields) do
                    checkexp(field.exp, st, errors)
                end
                node._type = types.Invalid()
            else
                local record = types.registry[context.fqtn]
                for _, cfield in ipairs(node.fields) do
                    local found
                    for _, rfield in ipairs(record.fields) do
                        if rfield.name == cfield.name then
                            found = rfield
                        end
                    end
                    if not found then
                        if cfield.name then
                            checker.typeerror(errors, cfield.loc,
                                "field '%s' not found in record type '%s'",
                                cfield.name, types.tostring(context))
                        else
                            checker.typeerror(errors, cfield.loc,
                                "missing field in initializer for record type '%s'",
                                types.tostring(context))
                        end
                        checkexp(cfield.exp, st, errors)
                    else
                        checkexp(cfield.exp, st, errors, found.type)
                        cfield.exp = trycoerce(cfield.exp, found.type, errors)
                        checkmatch("field " .. cfield.name,
                            found.type, cfield.exp._type, errors, cfield.loc)
                        cfield._field = found
                    end
                end
                for _, rfield in ipairs(record.fields) do
                    local found
                    for _, cfield in ipairs(node.fields) do
                        if rfield.name == cfield.name then
                            found = cfield
                        end
                    end
                    if not found then
                        checker.typeerror(errors, node.loc,
                            "field '%s' from record type '%s' missing in constructor",
                            rfield.name, types.tostring(context))
                    end
                end
                node._type = types.Nominal(context.fqtn)
            end
        else
            local isarray = true
            local ismap = false
            local ktype = types.Integer()
            local vtype = types.Integer()
            for _, field in ipairs(node.fields) do
                checkexp(field.exp, st, errors)
                vtype = field.exp._type
                isarray = isarray and not field.name
                if type(field.name) == "table" then
                    checkexp(field.name, st, errors)
                    ktype = field.name._type
                    ismap = true
                end
            end
            if ismap then
                node._type = types.Map(ktype, vtype)
            elseif isarray then
                node._type = types.Array(vtype)
            else
                node._type = types.Invalid()
            end
        end
    end,

    ["Ast.ExpVar"] = function(node, st, errors, context)
        checkvar(node.var, st, errors, context)
        if node.var._decl then node.var._decl._used = true end
        local texp = node.var._type
        if texp._tag == "Type.Module" then
            checker.typeerror(errors, node.loc,
                "trying to access module '%s' as a first-class value",
                node.var.name)
            node._type = types.Invalid()
        elseif texp._tag == "Type.Function" then
            checker.typeerror(errors, node.loc,
                "trying to access a function as a first-class value")
            node._type = types.Invalid()
        elseif texp._tag == "Type.Record" then
            checker.typeerror(errors, node.loc,
                "trying to access record type '%s' as a first-class value",
                texp.name)
            node._type = types.Invalid()
        else
            node._type = texp
        end
    end,

    ["Ast.ExpUnop"] = function(node, st, errors, context)
        local op = node.op
        checkexp(node.exp, st, errors)
        local texp = node.exp._type
        local loc = node.loc
        if op == "#" then
            if texp._tag ~= "Type.Array" and texp._tag ~= "Type.String" then
                checker.typeerror(errors, loc,
                    "trying to take the length of a %s instead of an array or string",
                    types.tostring(texp))
            end
            node._type = types.Integer()
        elseif op == "-" then
            if texp._tag ~= "Type.Integer" and texp._tag ~= "Type.Float" then
                checker.typeerror(errors, loc,
                    "trying to negate a %s instead of a number",
                    types.tostring(texp))
            end
            node._type = texp
        elseif op == "~" then
            -- always tries to coerce floats to integer
            node.exp = node.exp._type._tag == "Type.Float" and trycoerce(node.exp, types.Integer(), errors) or node.exp
            texp = node.exp._type
            if texp._tag ~= "Type.Integer" then
                checker.typeerror(errors, loc,
                    "trying to bitwise negate a %s instead of an integer",
                    types.tostring(texp))
            end
            node._type = types.Integer()
        elseif op == "not" then
            -- always coerces other values to a boolean
            node.exp = trycoerce(node.exp, types.Boolean(), errors)
            node._type = types.Boolean()
        else
            error("invalid unary operation " .. op)
        end
    end,

    ["Ast.ExpConcat"] = function(node, st, errors, context)
        for i, exp in ipairs(node.exps) do
            checkexp(exp, st, errors, types.String())
            -- always tries to coerce numbers to string
            exp = trytostr(exp)
            node.exps[i] = exp
            local texp = exp._type
            if texp._tag == "Type.Value" then
                checker.typeerror(errors, exp.loc,
                    "cannot concatenate with value of type 'value'")
            elseif texp._tag ~= "Type.String" then
                checker.typeerror(errors, exp.loc,
                    "cannot concatenate with %s value", types.tostring(texp))
            end
        end
        node._type = types.String()
    end,

    ["Ast.ExpBinop"] = function(node, st, errors, context)
        local op = node.op
        checkexp(node.lhs, st, errors)
        local tlhs = node.lhs._type
        checkexp(node.rhs, st, errors)
        local trhs = node.rhs._type
        local loc = node.loc
        if op == "==" or op == "~=" then
            -- tries to coerce to value if either side is value
            if tlhs._tag == "Type.Value" or trhs._tag == "Type.Value" then
                node.lhs = trycoerce(node.lhs, types.Value(), errors)
                tlhs = node.lhs._type
                node.rhs = trycoerce(node.rhs, types.Value(), errors)
                trhs = node.rhs._type
            end
            -- tries to coerce to float if either side is float
            if tlhs._tag == "Type.Float" or trhs._tag == "Type.Float" then
                node.lhs = trycoerce(node.lhs, types.Float(), errors)
                tlhs = node.lhs._type
                node.rhs = trycoerce(node.rhs, types.Float(), errors)
                trhs = node.rhs._type
            end
            if not types.compatible(tlhs, trhs) then
                checker.typeerror(errors, loc,
                    "trying to compare values of different types: %s and %s",
                    types.tostring(tlhs), types.tostring(trhs))

            end
            node._type = types.Boolean()
        elseif op == "<" or op == ">" or op == "<=" or op == ">=" then
            -- tries to coerce to value if either side is value
            if tlhs._tag == "Type.Value" or trhs._tag == "Type.Value" then
                node.lhs = trycoerce(node.lhs, types.Value(), errors)
                tlhs = node.lhs._type
                node.rhs = trycoerce(node.rhs, types.Value(), errors)
                trhs = node.rhs._type
            end
            -- tries to coerce to float if either side is float
            if tlhs._tag == "Type.Float" or trhs._tag == "Type.Float" then
                node.lhs = trycoerce(node.lhs, types.Float(), errors)
                tlhs = node.lhs._type
                node.rhs = trycoerce(node.rhs, types.Float(), errors)
                trhs = node.rhs._type
            end
            if not types.equals(tlhs, trhs) then
                if tlhs._tag ~= "Type.Integer" and tlhs._tag ~= "Type.Float" and trhs._tag == "Type.Integer" or trhs._tag == "Type.Float" then
                    checker.typeerror(errors, loc,
                        "left hand side of relational expression is a %s instead of a number",
                        types.tostring(tlhs))
                elseif trhs._tag ~= "Type.Integer" and trhs._tag ~= "Type.Float" and tlhs._tag == "Type.Integer" or tlhs._tag == "Type.Float" then
                    checker.typeerror(errors, loc,
                        "right hand side of relational expression is a %s instead of a number",
                        types.tostring(trhs))
                elseif tlhs._tag ~= "Type.String" and trhs._tag == "Type.String" then
                    checker.typeerror(errors, loc,
                        "left hand side of relational expression is a %s instead of a string",
                        types.tostring(tlhs))
                elseif trhs._tag ~= "Type.String" and tlhs._tag == "Type.String" then
                    checker.typeerror(errors, loc,
                        "right hand side of relational expression is a %s instead of a string",
                        types.tostring(trhs))
                else
                    checker.typeerror(errors, loc,
                        "trying to use relational expression with %s and %s",
                         types.tostring(tlhs), types.tostring(trhs))
                end
            else
                if tlhs._tag ~= "Type.Integer" and tlhs._tag ~= "Type.Float" and tlhs._tag ~= "Type.String" then
                    checker.typeerror(errors, loc,
                        "trying to use relational expression with two %s values",
                        types.tostring(tlhs))
                end
            end
            node._type = types.Boolean()
        elseif op == "+" or op == "-" or op == "*" or op == "%" or op == "//" then
            if not (tlhs._tag == "Type.Integer" or tlhs._tag == "Type.Float") then
                checker.typeerror(errors, loc,
                    "left hand side of arithmetic expression is a %s instead of a number",
                    types.tostring(tlhs))
            end
            if not (trhs._tag == "Type.Integer" or trhs._tag == "Type.Float") then
                checker.typeerror(errors, loc,
                    "right hand side of arithmetic expression is a %s instead of a number",
                    types.tostring(trhs))
            end
            -- tries to coerce to value if either side is value
            if tlhs._tag == "Type.Value" or trhs._tag == "Type.Value" then
                node.lhs = trycoerce(node.lhs, types.Value(), errors)
                tlhs = node.lhs._type
                node.rhs = trycoerce(node.rhs, types.Value(), errors)
                trhs = node.rhs._type
            end
            -- tries to coerce to float if either side is float
            if tlhs._tag == "Type.Float" or trhs._tag == "Type.Float" then
                node.lhs = trycoerce(node.lhs, types.Float(), errors)
                tlhs = node.lhs._type
                node.rhs = trycoerce(node.rhs, types.Float(), errors)
                trhs = node.rhs._type
            end
            if tlhs._tag == "Type.Float" and trhs._tag == "Type.Float" then
                node._type = types.Float()
            elseif tlhs._tag == "Type.Integer" and trhs._tag == "Type.Integer" then
                node._type = types.Integer()
            else
                -- error
                node._type = types.Invalid()
            end
        elseif op == "/" or op == "^" then
            if tlhs._tag == "Type.Integer" then
                -- always tries to coerce to float
                node.lhs = trycoerce(node.lhs, types.Float(), errors)
                tlhs = node.lhs._type
            end
            if trhs._tag == "Type.Integer" then
                -- always tries to coerce to float
                node.rhs = trycoerce(node.rhs, types.Float(), errors)
                trhs = node.rhs._type
            end
            if tlhs._tag ~= "Type.Float" then
                checker.typeerror(errors, loc,
                    "left hand side of arithmetic expression is a %s instead of a number",
                    types.tostring(tlhs))
            end
            if trhs._tag ~= "Type.Float" then
                checker.typeerror(errors, loc,
                    "right hand side of arithmetic expression is a %s instead of a number",
                    types.tostring(trhs))
            end
            node._type = types.Float()
        elseif op == "and" or op == "or" then
            -- tries to coerce to boolean if other side is boolean
            if tlhs._tag == "Type.Boolean" or trhs._tag == "Type.Boolean" then
                node.lhs = trycoerce(node.lhs, types.Boolean(), errors)
                tlhs = node.lhs._type
                node.rhs = trycoerce(node.rhs, types.Boolean(), errors)
                trhs = node.rhs._type
            end
            -- tries to coerce to value if other side is value
            if tlhs._tag == "Type.Value" or trhs._tag == "Type.Value" then
                node.lhs = trycoerce(node.lhs, types.Value(), errors)
                tlhs = node.lhs._type
                node.rhs = trycoerce(node.rhs, types.Value(), errors)
                trhs = node.rhs._type
            end
            -- tries to coerce to float if other side is float
            if tlhs._tag == "Type.Float" or trhs._tag == "Type.Float" then
              node.lhs = trycoerce(node.lhs, types.Float(), errors)
              tlhs = node.lhs._type
              node.rhs = trycoerce(node.rhs, types.Float(), errors)
              trhs = node.rhs._type
            end
            if not types.compatible(tlhs, trhs) then
              checker.typeerror(errors, loc,
                  "left hand side of logical expression is a %s but right hand side is a %s",
                   types.tostring(tlhs), types.tostring(trhs))
            end
            node._type = tlhs
        elseif op == "|" or op == "&" or op == "<<" or op == ">>" then
            -- always tries to coerce floats to integer
            node.lhs = node.lhs._type._tag == "Type.Float" and trycoerce(node.lhs, types.Integer(), errors) or node.lhs
            tlhs = node.lhs._type
            -- always tries to coerce floats to integer
            node.rhs = node.rhs._type._tag == "Type.Float" and trycoerce(node.rhs, types.Integer(), errors) or node.rhs
            trhs = node.rhs._type
            if tlhs._tag ~= "Type.Integer" then
                checker.typeerror(errors, loc,
                    "left hand side of arithmetic expression is a %s instead of a number",
                    types.tostring(tlhs))
            end
            if trhs._tag ~= "Type.Integer" then
                checker.typeerror(errors, loc,
                    "right hand side of arithmetic expression is a %s instead of a number",
                    types.tostring(trhs))
            end
            node._type = types.Integer()
        else
            error("invalid binary operation " .. op)
        end
    end,

    ["Ast.ExpCall"] = function(node, st, errors, context)
        if node.exp._tag == "Ast.ExpVar" then
            local var = node.exp.var
            checkvar(var, st, errors)
            if var._decl then var._decl._used = true end
            node.exp._type = var._type
        else
            checkexp(node.exp, st, errors)
        end
        if node.args._tag == "Ast.ArgsFunc" then
            local ftype = node.exp._type
            local fname = expname(node.exp)
            local var = node.exp.var
            if not (var and var._decl and (var._decl._tag == "Ast.TopLevelFunc" or
              var._decl._tag == "Type.ModuleMember" or
              var._decl._tag == "Type.StaticMethod")) then
                checker.typeerror(errors, node.loc,
                    "first-class functions are not supported in this version of Titan")
            end
            if ftype._tag ~= "Type.Function" then
                checker.typeerror(errors, node.loc,
                    "'%s' is not a function but %s",
                    fname, types.tostring(ftype))
                for _, arg in ipairs(node.args.args) do
                    checkexp(arg, st, errors)
                end
                node._type = types.Invalid()
                return
            end
            checkargs(ftype, node.args.args, node.loc,
                "function '" .. fname .. "'", st, errors)
            node._type = ftype.rettypes[1]
            node._types = ftype.rettypes
        else
            assert(node.args._tag == "Ast.ArgsMethod")
            local otype = node.exp._type
            if otype._tag == "Type.Nominal" and
               types.registry[otype.fqtn] and
               types.registry[otype.fqtn]._tag == "Type.Record" then
                local class = types.registry[otype.fqtn]
                if not class.methods[node.args.method] then
                    checker.typeerror(errors, node.loc,
                        "method '%s' not found in record '%s'",
                        node.args.method, types.tostring(otype))
                    for _, arg in ipairs(node.args.args) do
                        checkexp(arg, st, errors)
                    end
                    node._type = types.Invalid()
                    return
                end
                local ftype = class.methods[node.args.method]
                checkargs(ftype, node.args.args, node.loc,
                    "method '" .. class.name .. ":" .. node.args.method .. "'", st, errors)
                node._method = ftype
                node._type = ftype.rettypes[1]
                node._types = ftype.rettypes
            else
                if otype._tag ~= "Type.Nominal" then
                    checker.typeerror(errors, node.loc,
                        "expected record in receiver of method call but found '%s'",
                        types.tostring(otype))
                elseif not types.registry[otype.fqtn] then
                    checker.typeerror(errors, node.loc,
                        "record type '%s' in receiver of method call does not exist",
                        types.tostring(otype))
                elseif types.registry[otype.fqtn]._tag ~= "Type.Record" then
                    checker.typeerror(errors, node.loc,
                        "expected record in receiver of method call but found '%s'",
                        types.tostring(types.registry[otype.fqtn]))
                end
                for _, arg in ipairs(node.args.args) do
                    checkexp(arg, st, errors)
                end
                node._type = types.Invalid()
            end
        end
    end,

    ["Ast.ExpCast"] = function(node, st, errors, context)
        node.target = typefromnode(node.target, st, errors)
        checkexp(node.exp, st, errors, node.target)
        if not (types.explicitly_coerceable(node.exp._type, node.target) or
                types.coerceable(node.exp._type, node.target))
           or not types.compatible(node.exp._type, node.target) then
            checker.typeerror(errors, node.loc, "cannot cast '%s' to '%s'",
                types.tostring(node.exp._type), types.tostring(node.target))
        end
        node._type = node.target
    end,

    ["Ast.ExpAdjust"] = function(node, st, errors, context)
        checkexp(node.exp, st, errors, context)
        node._type = node.exp._type
    end
})

--
-- TopLevel
--

-- Typechecks a function body
--   node: TopLevelFunc AST node
--   st: symbol table
--   errors: list of compile-time errors
local function checkfunc(node, st, errors)
    st:add_symbol("$function", node) -- for return type
    local ptypes = node._type.params
    local pnames = {}
    for i, rettype in ipairs(node._type.rettypes) do
        checktype(rettype, node.rettypes[i].loc, errors)
    end
    for i, param in ipairs(node.params) do
        st:add_symbol(param.name, param)
        param._type = ptypes[i]
        checktype(param._type, param.loc, errors)
        if pnames[param.name] then
            checker.typeerror(errors, node.loc,
                "duplicate parameter '%s' in declaration of function '%s'",
                param.name, node.name)
        else
            pnames[param.name] = true
        end
    end
    local ret = st:with_block(checkstat, node.block, st, errors)
    if not ret and node._type.rettypes[1]._tag ~= "Type.Nil" then
        checker.typeerror(errors, node.loc,
            "function can return nil but return type is not nil")
    end
end

-- Typechecks a method body, binding self to the first parameter
--   node: TopLevelMethod AST node
--   st: symbol table
--   errors: list of compile-time errors
local function checkmethod(node, st, errors)
    local self = ast.Decl(node.loc, "self", ast.TypeName(node.loc, node._record.name))
    node._self = self
    self._type = typefromnode(self.type, st, errors)
    st:add_symbol("self", self)
    checkfunc(node, st, errors)
end

-- Checks function bodies
--   ast: AST for the whole module
--   st: symbol table
--   errors: list of compile-time errors
local function checkbodies(ast, st, errors)
    for _, node in ipairs(ast) do
        if not node._ignore then
            if node._tag == "Ast.TopLevelFunc" then
                st:with_block(checkfunc, node, st, errors)
            elseif node._tag == "Ast.TopLevelMethod" then
                st:with_block(checkmethod, node, st, errors)
            elseif node._tag == "Ast.TopLevelStatic" then
                st:with_block(checkfunc, node, st, errors)
            elseif node._tag == "Ast.TopLevelRecord" then
                local fields = node._type.fields
                for i, field in ipairs(fields) do
                    local ftype = field.type
                    checktype(ftype, node.fields[i].loc, errors)
                end
            end
        end
    end
end

local function isconstructor(node)
    return node.var and node.var._decl and node.var._decl._tag == "Type.StaticMethod"
        and #node.var._decl.rettypes == 1
        and node.var._decl.rettypes[1]._tag == "Type.Nominal"
        and node.var._decl.rettypes[1].fqtn == node.var._decl.fqtn
end

-- Verify if an expression is constant
local function isconst(node)
    local tag = node._tag
    if tag == "Ast.ExpNil" or
       tag == "Ast.ExpBool" or
       tag == "Ast.ExpInteger" or
       tag == "Ast.ExpFloat" or
       tag == "Ast.ExpString" then
        return true

    elseif tag == "Ast.ExpInitList" then
        local const = true
        for _, field in ipairs(node.fields) do
            const = const and isconst(field.exp)
        end
        return const

    elseif tag == "Ast.ExpCall" then
        if isconstructor(node.exp) then
            local const = true
            for _, arg in ipairs(node.args) do
                const = const and isconst(arg)
            end
            return const
        else
            return false
        end

    elseif tag == "Ast.ExpVar" then
        return false

    elseif tag == "Ast.ExpConcat" then
        local const = true
        for _, exp in ipairs(node.exps) do
            const = const and isconst(exp)
        end
        return const

    elseif tag == "Ast.ExpUnop" then
        return isconst(node.exp)

    elseif tag == "Ast.ExpBinop" then
        return isconst(node.lhs) and isconst(node.rhs)

    elseif tag == "Ast.ExpCast" then
        return isconst(node.exp)

    else
        error("impossible")
    end
end

-- Return the name given the toplevel node
local function toplevel_name(node)
    local tag = node._tag
    if tag == "Ast.TopLevelImport" then
        return node.localname
    elseif tag == "Ast.TopLevelForeignImport" then
        return node.localname
    elseif tag == "Ast.TopLevelVar" then
        return node.decl.name
    elseif tag == "Ast.TopLevelFunc" or
           tag == "Ast.TopLevelRecord" then
        return node.name
    elseif tag == "Ast.TopLevelMethod" or tag == "Ast.TopLevelStatic" then
        return nil
    else
        error("tag not found " .. tag)
    end
end

-- Typecheck the toplevel node
local toplevel_visitor = util.make_visitor({
    ["Ast.TopLevelImport"] = function(node, st, errors, loader)
        local modtype, errs = checker.checkimport(node.modname, loader)
        if modtype then
            node._type = modtype
            for _, err in ipairs(errs) do
                table.insert(errors, err)
            end
        else
            node._type = types.Invalid()
            checker.typeerror(errors, node.loc,
                "problem loading module '%s': %s",
                node.modname, errs)
        end
    end,

    ["Ast.TopLevelForeignImport"] = function(node, st, errors)
        local name = node.headername
        local ftypes, defines = cdriver.process_file(name)
        if ftypes then
            local members = {}
            for _, item in ipairs(ftypes) do
                local fname = item.name
                local ftype = item.type
                local decl, err = foreigntypes.convert(st, ftype)
                if decl then
                    members[fname] = types.ModuleMember(name, fname, decl)
                    st:add_foreign_type(fname, decl)
                else
                    checker.typeerror(errors, err, node._pos)
                end
            end
            local modtype = types.ForeignModule(name, members)
            node._type = modtype
        else
            node._type = types.Nil()
            checker.typeerror(errors, node.loc, defines)
        end
        st:add_symbol(node.localname, node)
    end,

    ["Ast.TopLevelVar"] = function(node, st, errors)
        if node.decl.type then
            node._type = typefromnode(node.decl.type, st, errors)
            checkexp(node.value, st, errors, node._type)
            node.value = trycoerce(node.value, node._type, errors)
            checkmatch("declaration of module variable " .. node.decl.name,
                       node._type, node.value._type, errors, node.loc)
        else
            checkexp(node.value, st, errors)
            node._type = node.value._type
        end
        if not isconst(node.value) then
            checker.typeerror(errors, node.value.loc,
                "top level variable initialization must be constant")
        end
    end,

    ["Ast.TopLevelFunc"] = function(node, st, errors)
        local ptypes = {}
        for _, pdecl in ipairs(node.params) do
            table.insert(ptypes, typefromnode(pdecl.type, st, errors))
        end
        local rettypes = {}
        for _, rt in ipairs(node.rettypes) do
            table.insert(rettypes, typefromnode(rt, st, errors))
        end
        node._type = types.Function(ptypes, rettypes, false)
    end,

    ["Ast.TopLevelRecord"] = function(node, st, errors)
        local fields = {}
        local nameset = {}
        local fqtn = st.modname .. "." .. node.name
        local ftypes = {}
        for i, field in ipairs(node.fields) do
            if nameset[field.name] then
                checker.typeerror(errors, field.loc,
                "redeclaration of field '%s' in record '%s'",
                field.name, node.name)
            else
                nameset[field.name] = true
                local ftype = typefromnode(field.type, st, errors)
                table.insert(ftypes, ftype)
                table.insert(fields,
                    types.Field(fqtn, field.name, ftype, i))
            end
        end
        node._type = types.Record(fqtn, fields, {}, {})
        types.registry[fqtn] = node._type
    end,

    ["Ast.TopLevelStatic"] = function(node, st, errors)
        local record = st:find_symbol(node.class)
        if not record then
            node._ignore = true
            checker.typeerror(errors, node.loc,
                "record referenced by static method declaration '%s.%s' does not exist",
                node.class, node.name)
            return
        end
        if record._tag ~= "Ast.TopLevelRecord" then
            node._ignore = true
            checker.typeerror(errors, node.loc,
                "member referenced by static method declaration '%s.%s' is not a record",
                node.class, node.name)
            return
        end
        local rectype = record._type
        for _, field in ipairs(rectype.fields) do
            if field.name == node.name then
                node._ignore = true
                checker.typeerror(errors, node.loc,
                "cannot declare static method '%s.%s' as field '%s' exists in record '%s'",
                    node.class, node.name, field.name, node.class)
                return
            end
        end
        if rectype.methods[node.name] then
            node._ignore = true
            checker.typeerror(errors, node.loc,
                "cannot declare static method '%s.%s' as method '%s:%s' exists in record '%s'",
                    node.class, node.name, node.class, node.name, node.class)
            return
        end
        if rectype.functions[node.name] then
            node._ignore = true
            checker.typeerror(errors, node.loc,
                "redeclaration of static method '%s.%s'", node.class, node.name)
            return
        end
        node._record = record
        local ptypes = {}
        for _, pdecl in ipairs(node.params) do
            table.insert(ptypes, typefromnode(pdecl.type, st, errors))
        end
        local rettypes = {}
        for _, rt in ipairs(node.rettypes) do
            table.insert(rettypes, typefromnode(rt, st, errors))
        end
        node._type = types.StaticMethod(rectype.name, node.name, ptypes, rettypes)
        rectype.functions[node.name] = node._type
    end,

    ["Ast.TopLevelMethod"] = function(node, st, errors)
        local record = st:find_symbol(node.class)
        if not record then
            node._ignore = true
            checker.typeerror(errors, node.loc,
                "record referenced by method declaration '%s:%s' does not exist",
                node.class, node.name)
            return
        end
        if record._tag ~= "Ast.TopLevelRecord" then
            node._ignore = true
            checker.typeerror(errors, node.loc,
                "member referenced by method declaration '%s:%s' is not a record",
                node.class, node.name)
            return
        end
        local rectype = record._type
        for _, field in ipairs(rectype.fields) do
            if field.name == node.name then
                node._ignore = true
                checker.typeerror(errors, node.loc,
                "cannot declare method '%s:%s' as field '%s' exists in record '%s'",
                    node.class, node.name, field.name, node.class)
                return
            end
        end
        if rectype.functions[node.name] then
            node._ignore = true
            checker.typeerror(errors, node.loc,
                "cannot declare method '%s:%s' as static method '%s.%s' exists in record '%s'",
                    node.class, node.name, node.class, node.name, node.class)
            return
        end
        if rectype.methods[node.name] then
            node._ignore = true
            checker.typeerror(errors, node.loc,
                "redeclaration of method '%s:%s'", node.class, node.name)
            return
        end
        node._record = record
        local ptypes = {}
        for _, pdecl in ipairs(node.params) do
            table.insert(ptypes, typefromnode(pdecl.type, st, errors))
        end
        local rettypes = {}
        for _, rt in ipairs(node.rettypes) do
            table.insert(rettypes, typefromnode(rt, st, errors))
        end
        node._type = types.Method(rectype.name, node.name, ptypes, rettypes)
        rectype.methods[node.name] = node._type
    end,
})

-- Colect type information of toplevel nodes
--   ast: AST for the whole module
--   st: symbol table
--   errors: list of compile-time errors
--   annotates the top-level nodes with their types in a "_type" field
--   annotates whether a top-level declaration is duplicated with a "_ignore"
--   field
local function checktoplevel(ast, st, errors, loader)
    for _, node in ipairs(ast) do
        local name = toplevel_name(node)
        local dup = name and st:find_dup(name)
        if dup then
            checker.typeerror(errors, node.loc,
                "duplicate declaration for %s, previous one at line %d",
                name, dup.loc.line)
            node._ignore = true
        else
            toplevel_visitor(node, st, errors, loader)
            if name and not node._ignore then
                st:add_symbol(name, node)
            end
        end
    end
end

function checker.checkimport(modname, loader)
    local ok, type_or_error, errors = loader(modname)
    if not ok then return nil, type_or_error end
    for name, type in pairs(type_or_error.members) do
        if type._tag == "Type.Record" or type._tag == "Type.Interface" then
            types.registry[modname .. "." .. name] = type
        end
    end
    return type_or_error, errors
end

-- Builds a type for the module from the types of its public members
--   modast: AST for the module
--   returns "Type.Module" type
local function makemoduletype(modname, modast)
    local members = {}
    for _, tlnode in ipairs(modast) do
        if tlnode._tag ~= "Ast.TopLevelImport" and not tlnode.islocal and not tlnode._ignore then
            local tag = tlnode._tag
            if tag == "Ast.TopLevelFunc" then
                members[tlnode.name] = types.ModuleMember(modname, tlnode.name, tlnode._type)
            elseif tag == "Ast.TopLevelVar" then
                members[tlnode.decl.name] = types.ModuleMember(modname, tlnode.decl.name, tlnode._type)
            elseif tag == "Ast.TopLevelRecord" then
                members[tlnode.name] = types.ModuleMember(modname, tlnode.name, tlnode._type)
            elseif tag == "Ast.TopLevelInterface" then
                members[tlnode.name] = types.ModuleMember(modname, tlnode.name, tlnode._type)
            end
        end
    end
    return types.Module(modname, members)
end


-- Entry point for the typechecker
--   ast: AST for the whole module
--   subject: the string that generated the AST
--   filename: the file name that contains the subject
--   loader: the module loader, a function from module name to its AST, code,
--   and filename or nil and an error
--
--   returns true if typechecking succeeds, or false and a list of type errors
--   found
--   annotates the AST with the types of its terms in "_type" fields
--   annotates duplicate top-level declarations with a "_ignore" boolean field
function checker.check(modname, ast, subject, filename, loader)
    loader = loader or function ()
        return nil, "you must pass a loader to import modules"
    end
    local st = symtab.new(modname)
    local errors = {subject = subject, filename = filename}
    checktoplevel(ast, st, errors, loader)
    checkbodies(ast, st, errors)
    ast._type = makemoduletype(modname, ast)
    return ast._type, errors
end

return checker
