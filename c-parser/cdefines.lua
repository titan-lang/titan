
local cdefines = {}

local c99 = require("c-parser.c99")
local cpp = require("c-parser.cpp")

local function add_type(lst, name, typ)
    lst[name] = typ
    table.insert(lst, { name = name, type = typ })
end

local base_c_types = {
    CONST_CHAR_PTR = { "const", "char", "*" },
    CONST_CHAR = { "const", "char" },
    LONG_LONG = { "long", "long" },
    LONG = { "long" },
    DOUBLE = { "double" },
    INT = { "int" },
}

local function get_binop_type(e1, e2)
    if e1[1] == "double" or e2[1] == "double" then
        return base_c_types.DOUBLE
    end
    if e1[2] == "long" or e2[2] == "long" then
        return base_c_types.LONG_LONG
    end
    if e1[1] == "long" or e2[1] == "long" then
        return base_c_types.LONG
    end
    return base_c_types.INT
end

local binop_set = {
    ["+"] = true,
    ["-"] = true,
    ["*"] = true,
    ["/"] = true,
    ["%"] = true,
}

local relop_set = {
    ["<"] = true,
    [">"] = true,
    [">="] = true,
    ["<="] = true,
    ["=="] = true,
    ["!="] = true,
}

local bitop_set = {
    ["<<"] = true,
    [">>"] = true,
    ["&"] = true,
    ["^"] = true,
    ["|"] = true,
}

-- Best-effort assessment of the type of a #define
local function get_type_of_exp(exp, lst)
    if type(exp) == "string" then
        if exp:sub(1,1) == '"' or exp:sub(1,2) == 'L"' then
            return base_c_types.CONST_CHAR_PTR
        elseif exp:sub(1,1) == "'" or exp:sub(1,2) == "L'" then
            return base_c_types.CONST_CHAR
        elseif exp:match("^[0-9]*LL$") then
            return base_c_types.LONG_LONG
        elseif exp:match("^[0-9]*L$") then
            return base_c_types.LONG
        elseif exp:match("%.") then
            return base_c_types.DOUBLE
        else
            return base_c_types.INT
        end
    end

    assert(type(exp) == "table")

    if type(exp[1]) == "string" and exp[2] and exp[2].args then
        local fn = lst[exp[1]]
        if not fn then
            return nil -- unknown function
        end
        local r = fn.ret.type
        return table.move(r, 1, #r, 1, {}) -- shallow_copy(r)
    end

    if exp.unop == "*" then
        local etype = get_type_of_exp(exp[1], lst)
        if not etype then
            return
        end
        local rem = table.remove(etype)
        assert(rem == "*")
        return etype
    elseif exp.unop == "-" then
        return get_type_of_exp(exp[1], lst)
    elseif exp.op == "?" then
        return get_type_of_exp(exp[2], lst)
    elseif binop_set[exp.op] then
        local e1 = get_type_of_exp(exp[1], lst)
        if not e1 then
            return
        end
        local e2 = get_type_of_exp(exp[2], lst)
        if not e2 then
            return
        end
        return get_binop_type(e1, e2)
    elseif relop_set[exp.op] then
        return base_c_types.INT
    elseif bitop_set[exp.op] then
        return get_type_of_exp(exp[1], lst) -- ...or should it be int?
    elseif exp.op then
        print("FIXME unsupported op", exp.op)
    end
end

function cdefines.register_define(define, lst)
    local name = define.name
    local def = define.def
    if #def == 0 then
        return nil
    end

    local text = table.concat(def, " ") .. " "
    local exp, err, rest = c99.match_language_expression_grammar(text)
    if not exp then
        -- failed parsing expression
        return
    end
    exp = cpp.remove_wrapping_subtables(exp)

    local typ = get_type_of_exp(exp, lst, name)
    if typ then
        add_type(lst, name, { type = typ })
    end
end

function cdefines.register_defines(lst, define_list)
    for _, define in ipairs(define_list) do
        cdefines.register_define(define, lst)
    end
end

return cdefines
