-- Module to convert c-parser types into Titan types
local foreigntypes = {}

local types = require "titan-compiler.types"
local inspect = require "inspect"

local function type_from_name(st, typename)
    for i = #typename, 1, -1 do
        local token = typename[i]
        if token == "const"
        or token == "restrict" then
            table.remove(typename, i)
        end
    end
    if #typename == 1 then
        local itype = st:find_foreign_type(typename[1])
        if itype then
            return itype
        elseif typename[1] == "void" then
            return types.Nil()
        elseif typename[1] == "int" then
            return types.Integer()
        elseif typename[1] == "long" then
            return types.Integer()
        elseif typename[1] == "double" then
            return types.Float()
        end
    elseif #typename == 2 and typename[1] == "char" and typename[2] == "*" then
        return types.String()
    elseif typename[#typename] == "*" then
        table.remove(typename)
        return {
            _tag = "Type.Pointer",
            type = type_from_name(st, typename)
        }
    end
    return { _tag = "Type.Unknown", data = typename }
end

local function convert_function(st, ftype)
    local decl = {
        islocal = true,
        isforeign = true,
        name = ftype.name,
    }
    local ptypes = {}
    for _, pdecl in ipairs(ftype.params) do
        table.insert(ptypes, foreigntypes.convert(st, pdecl))
    end
    local rettypes = {}
    table.insert(rettypes, foreigntypes.convert(st, ftype.ret))
    decl._type = types.Function(ptypes, rettypes, ftype.vararg)
    decl.params = ptypes
    decl.rettypes = rettypes
    return decl._type
end

local function convert_typedef(st, ftype)
    local decl = {
        _tag = "Type.Typedef",
        name = ftype.name,
    }
    local ttype
    local longs = 0
    for _, item in ipairs(ftype.def) do
        if type(item) == "table" then
            ttype = foreigntypes.convert(st, item)
        else
            local itype = st:find_foreign_type(item)
            if itype then
                ttype = itype
            elseif item == "long" then
                longs = longs + 1
            elseif item == "double" then
                if longs > 0 then
                    error("NYI long double")
                end
                ttype = types.Float()
            elseif item == "int" then
                ttype = types.Integer()
            elseif item == "char" then
                ttype = types.Integer()
            elseif item == "short" then
                ttype = types.Integer()
            end
        end
    end
    if not ttype and longs > 0 then
        ttype = types.Integer()
    end
    if ttype then
        decl._type = ttype
        return decl
    end
    --decl.data = ftype
    return decl
end

local function convert_struct(st, ftype)
    local decl = {
        _tag = "Struct",
        name = ftype.name,
        data = ftype,
    }
    -- TODO process fields
    return decl
end

local function convert_union(st, ftype)
    local decl = {
        _tag = "Union",
        name = ftype.name,
        data = ftype,
    }
    -- TODO process fields
    return decl
end

local function convert_enum(st, ftype)
    local decl = {
        _tag = "Enum",
        name = ftype.name,
        data = ftype,
    }
    -- TODO process fields
    return decl
end

function foreigntypes.convert(st, ftype)
    if ftype.type == "function" then
        return convert_function(st, ftype)
    elseif ftype.type == "typedef" then
        return convert_typedef(st, ftype)
    elseif ftype.type == "struct" then
        return convert_struct(st, ftype)
    elseif ftype.type == "union" then
        return convert_union(st, ftype)
    elseif ftype.type == "enum" then
        return convert_enum(st, ftype)
    elseif type(ftype.type) == "table" and ftype.type[1] then
        return type_from_name(st, ftype.type)
    else
        return nil, "FIXME unconverted " .. inspect(ftype)
    end
end

return foreigntypes

