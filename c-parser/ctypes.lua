
local ctypes = {}

local util = require("titan-compiler.util")
local inspect = require("inspect")

local equal_declarations

local function add_type(lst, name, typ)
    lst[name] = typ
    table.insert(lst, { name = name, type = typ })
end

-- Compare two strings or two lists of declarations
local function equal_lists(l1, l2)
    if type(l1) == "string" or type(l1) == "nil" then
        return l1 == l2
    end

    if #l1 ~= #l2 then
        return false
    end
    for i, p1 in ipairs(l1) do
        local p2 = l2[i]
        if not equal_declarations(p1, p2) then
            return false
        end
    end
    return true
end

equal_declarations = function(t1, t2)
    if type(t1) == "string" or type(t2) == "nil" then
        return t1 == t2
    end
    if not equal_declarations(t1.type, t2.type) then
        return false
    end
--    if not equal_lists(t1.name, t2.name) then
--        return false
--    end
    if t1.type == "struct" then
        if t1.name ~= t2.name then
            return false
        end
    elseif t1.type == "function" then
        if not equal_declarations(t1.ret.type, t2.ret.type) then
            return false
        end
        if not equal_lists(t1.params, t2.params) then
            return false
        end
        if t1.vararg ~= t2.vararg then
            return false
        end
    end
    return true
end

local function get_name(name_src)
    local ret_pointer = {}
    if name_src == nil then
        return nil, "could not find a name: " .. inspect(name_src)
    end
    local name
    if type(name_src) == "string" then
        name = name_src
    else
        while name_src[1] do
            table.insert(ret_pointer, table.remove(name_src, 1))
        end
        name = name_src.name
    end
    if not type(name) == "string" then
        return nil, "failed finding name: " .. inspect(name_src)
    end
    return name, ret_pointer
end

local get_type

local function get_field(lst, field_src)
    local name, ret_pointer
    if type(field_src) == "string" then
        name = nil
        field_src = { field_src }
        ret_pointer = {}
    elseif type(field_src) == "table" and field_src.ids.name then
        -- FIXME multiple ids, e.g.: int *x, y, *z;
        name, ret_pointer = get_name(field_src.ids)
        if not name then
            return nil, ret_pointer
        end
    else
        name = nil
        ret_pointer = {}
    end

    local typ, err = get_type(lst, field_src, ret_pointer)
    if not typ then
        return nil, err
    end

    return {
        name = name,
        type = typ
    }
end

local function get_fields(lst, fields_src)
    local fields = {}
    if not fields_src[1] then
        fields_src = { fields_src }
    end
    for _, field_src in ipairs(fields_src) do
        local field, err = get_field(lst, field_src)
        if not field then
            return nil, err
        end
        table.insert(fields, field)
    end
    return fields
end

local function get_structunion(lst, spec)
    local name = spec.id
    local key = spec.type .. "@"..(name or tostring(spec))

    if not lst[key] then
        -- "Forward declaration" for recursive structs
        lst[key] = {
            type = spec.type,
            name = name,
        }
    end

    local fields, err
    if spec.fields then
        fields, err = get_fields(lst, spec.fields)
        if not fields then
            return nil, err
        end
    end

    local typ = {
        type = spec.type,
        name = name,
        fields = fields,
    }

    if lst[key] then
        if lst[key].fields then
            return nil, "redeclaration for " .. typ.name
        end
    end
    add_type(lst, key, typ)

    return typ, key
end

local function get_enum(lst, spec)
    local name = spec.id
    local key = spec.type .. "@"..(name or tostring(spec))
--
--    if not lst[key] then
--        -- "Forward declaration" for recursive structs
--        lst[key] = {
--            type = spec.type,
--            name = name,
--        }
--    end

    local fields, err
    if spec.fields then
        fields, err = get_fields(lst, spec.fields)
        if not fields then
            return nil, err
        end
    end

    local typ = {
        type = spec.type,
        name = name,
        fields = fields,
    }

    if lst[key] then
        if lst[key].fields then
            return nil, "redeclaration for " .. typ.name
        end
    end
    add_type(lst, key, typ)

    return typ, key
end

local function refer(lst, item, get_fn)
    if item.id and not item.fields then
        local key = item.type .. "@" .. item.id
        local su_typ = lst[key]
        if not su_typ then
            return {
                type = item.type,
                name = { item.id },
            }
        end
        return su_typ
    else
        local typ, key = get_fn(lst, item)
        if not typ then
            return nil, key
        end
        return typ
    end
end

local calculate

local function binop(val, fn)
    local e1, e2 = calculate(val[1]), calculate(val[2])
    if type(e1) == "number" and type(e2) == "number" then
        return fn(e1, e2)
    else
        return { e1, e2, op = val.op }
    end
end

calculate = function(val)
    if type(val) == "string" then
        return tonumber(val)
    end
    if val.op == "+" then
        return binop(val, function(a, b) return a + b end)
    elseif val.op == "-" then
        return binop(val, function(a, b) return a - b end)
    elseif val.op == "*" then
        return binop(val, function(a, b) return a * b end)
    elseif val.op == "/" then
        return binop(val, function(a, b) return a / b end)
    else
        return val
    end
end

local base_types = {
    ["char"] = true,
    ["const"] = true,
    ["double"] = true,
    ["float"] = true,
    ["int"] = true,
    ["long"] = true,
    ["short"] = true,
    ["signed"] = true,
    ["unsigned"] = true,
    ["void"] = true,
    ["volatile"] = true,
    ["_Bool"] = true,
    ["*"] = true,
}

get_type = function(lst, spec, ret_pointer)
    local typ = {}
    if type(spec.type) == "string" then
        spec.type = { spec.type }
    end
    if spec.type and not spec.type[1] then
        spec.type = { spec.type }
    end
    for _, part in ipairs(spec.type or spec) do
        if part == "extern" then
            -- skip
        elseif part == "typedef" then
            -- skip
        elseif part == "restrict" then
            -- skip
        elseif base_types[part] then
            table.insert(typ, part)
        elseif lst[part] and lst[part].type == "typedef" then
            table.insert(typ, part)
        elseif type(part) == "table" and part.type == "struct" or part.type == "union" then
            local su_typ, err = refer(lst, part, get_structunion)
            if not su_typ then
                return nil, err or "failed to refer struct"
            end
            table.insert(typ, su_typ)
        elseif type(part) == "table" and part.type == "enum" then
            local en_typ, err = refer(lst, part, get_enum)
            if not en_typ then
                return nil, err or "failed to refer enum"
            end
            table.insert(typ, en_typ)
        else
            return nil, "FIXME unknown type " .. inspect(spec)
        end
    end
    if #ret_pointer > 0 then
        for _, item in ipairs(ret_pointer) do
            if type(item) == "table" and item.idx then
                table.insert(typ, { idx = calculate(item.idx) })
            else
                table.insert(typ, item)
            end
        end
    end
    return typ
end

local function get_param(lst, param_src)
    local name, ret_pointer
    if type(param_src) == "string" then
        name = nil
        param_src = { param_src }
        ret_pointer = {}
    elseif type(param_src.id) == "table" and param_src.id.name then
        name, ret_pointer = get_name(param_src.id)
        if not name then
            return nil, ret_pointer
        end
    else
        name = nil
        ret_pointer = {}
    end

    local typ, err = get_type(lst, param_src, ret_pointer)
    if not typ then
        return nil, err
    end

    return {
        name = name,
        type = typ
    }
end

local function get_params(lst, params_src)
    local params = {}
    local vararg = false

    if params_src.param then
        params_src = { params_src }
    end
    for _, param_src in ipairs(params_src) do
        if param_src == "..." then
            vararg = true
        else
            local param, err = get_param(lst, param_src.param)
            if not param then
                return nil, err
            end
            table.insert(params, param)
        end
    end
    return params, vararg
end

local function register_extern(lst, item, spec_set)
    local name, ret_pointer = get_name(item.ids.decl)
    if not name then
        return nil, ret_pointer
    end
    local ret_type, err = get_type(lst, item.spec, ret_pointer)
    if not ret_type then
        return nil, err
    end
    local params
    local vararg = false
    if item.ids.decl.params then
        params, vararg = get_params(lst, item.ids.decl.params)
        if not params then
            return nil, vararg
        end
    else
        params = {}
    end

    local typ = {
        type = "function",
        name = name,
        ret = {
            type = ret_type,
        },
        params = params,
        vararg = vararg,
    }
    if lst[name] then
        if not equal_declarations(lst[name], typ) then
            return nil, "inconsistent declaration for " .. name .. " - " .. inspect(lst[name]) .. " VERSUS " .. inspect(typ)
        end
    end
    add_type(lst, name, typ)

    return true
end

local function register_static_function(lst, item)
    return true
end

local function register_typedef(lst, item)
    local name, ret_pointer = get_name(item.ids.decl.declarator or item.ids.decl)
    if not name then
        return nil, ret_pointer
    end
    local def, err = get_type(lst, item.spec, ret_pointer)
    if not def then
        return nil, err
    end
    local typ = {
        type = "typedef",
        name = name,
        def = def,
    }

    if lst[name] then
        if not equal_declarations(lst[name], typ) then
            return nil, "inconsistent declaration for " .. name .. " - " .. inspect(lst[name]) .. " VERSUS " .. inspect(typ)
        end
    end
    add_type(lst, name, typ)

    return true
end

local function register_structunion(lst, item)
    local typ, key = get_structunion(lst, item.spec)
    if not typ then
        return nil, key
    end
    return true
end

local function register_enum(lst, item)
    return true
end

function ctypes.register_types(parsed)
    local lst = {}
    if parsed.spec then
        parsed = { parsed }
    end
    for _, item in ipairs(parsed) do
        if type(item.spec) == "string" then
            item.spec = { item.spec }
        end
        local spec_set = util.to_set(item.spec)
        if spec_set.extern then
            local ok, err = register_extern(lst, item, spec_set)
            if not ok then
                return nil, err or "failed extern"
            end
        elseif spec_set.static and item.func then
            local ok, err = register_static_function(lst, item)
            if not ok then
                return nil, err or "failed static function"
            end
        elseif spec_set.typedef then
            local ok, err = register_typedef(lst, item)
            if not ok then
                return nil, err or "failed typedef"
            end
        elseif item.spec.type == "struct" or item.spec.type == "union" then
            local ok, err = register_structunion(lst, item)
            if not ok then
                return nil, err or "failed struct/union"
            end
        elseif item.spec.type == "enum" then
            local ok, err = register_enum(lst, item)
            if not ok then
                return nil, err or "failed enum"
            end
        elseif item.ids.decl.params then
            -- a function declared without extern
            local ok, err = register_extern(lst, item, spec_set)
            if not ok then
                return nil, err or "failed extern"
            end
        else
            return nil, "FIXME Uncategorized declaration: " .. inspect(item)
        end
    end
    return lst
end

return ctypes
