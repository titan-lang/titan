local cdriver = {}

local cpp = require("c-parser.cpp")
local c99 = require("c-parser.c99")
local ctypes = require("c-parser.ctypes")
local util = require("titan-compiler.util")

function cdriver.process_file(filename)
    local ctx, err = cpp.parse_file(filename)
    if not ctx then
        return nil, "failed preprocessing '"..filename.."': " .. err
    end

    local srccode = table.concat(ctx.output, "\n").."<EOF>"

    local res, err, rest = c99.match_language_grammar(srccode)
    if not res then
        local line, col = util.get_line_number(srccode, (#srccode - #rest) + 1)
        return nil, ("failed parsing: %s:%d:%d: %s\n"):format(filename, line, col, err)
    end

    res = cpp.remove_wrapping_subtables(res)

    local ffi_types, err = ctypes.register_types(res)
    if not ffi_types then
        return nil, err
    end
    return ffi_types, ctx.defines
end

return cdriver
