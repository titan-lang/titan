
local exps = {}

local c99 = require("c-parser.c99")
local typed = require("typed")

local shl, shr, bor
if jit then
    shl = function(a, b)
        return bit.lshift(a, b)
    end
    shr = function(a, b)
        return bit.rshift(a, b)
    end
    bor = function(a, b)
        return bit.bor(a, b)
    end
else
    shl, shr, bor = load([[
        local function shl(a, b)
            return a << b
        end
        local function shr(a, b)
            return a >> b
        end
        local function bor(a, b)
            return a | b
        end
        return shl, shr, bor
    ]])()
end

local parse_expression = typed("{string} -> Exp?", function(tokens)
    local text = table.concat(tokens, " ")
    local exp, err, _, _, fragment = c99.match_preprocessing_expression_grammar(text)
    if not exp then
        print("Error parsing expression: " .. tostring(err) .. ": " .. text .. " AT " .. fragment)
    end
    return exp
end)

local eval_exp
eval_exp = typed("table?, Exp -> number", function(defs, exp)
    if not exp.op then
        local val = exp[1]
        typed.check(val, "string")
        if defs then
            local defined = defs[val]
            if defined then
                assert(type(defined) == "table")
                local subexp = parse_expression(defined)
                if not subexp then
                    return 0 -- FIXME
                end
                return eval_exp(defs, subexp)
            end
        end
        val = val:gsub("U*L*$", "")
        if val:match("^0[xX]") then
            return tonumber(val) or 0
        elseif val:sub(1,1) == "0" then
            return tonumber(val, 8) or 0
        else
            return tonumber(val) or 0
        end
    elseif exp.op == "+"  then
        if exp[2] then
            return eval_exp(defs, exp[1]) + eval_exp(defs, exp[2])
        else
            return eval_exp(defs, exp[1])
        end
    elseif exp.op == "-"  then
        if exp[2] then
            return eval_exp(defs, exp[1]) - eval_exp(defs, exp[2])
        else
            return -(eval_exp(defs, exp[1]))
        end
    elseif exp.op == "*"  then return eval_exp(defs, exp[1]) * eval_exp(defs, exp[2])
    elseif exp.op == "/"  then return eval_exp(defs, exp[1]) / eval_exp(defs, exp[2])
    elseif exp.op == ">>" then return shr(eval_exp(defs, exp[1]), eval_exp(defs, exp[2])) -- FIXME C semantics
    elseif exp.op == "<<" then return shl(eval_exp(defs, exp[1]), eval_exp(defs, exp[2])) -- FIXME C semantics
    elseif exp.op == "|" then return bor(eval_exp(defs, exp[1]), eval_exp(defs, exp[2])) -- FIXME C semantics
    elseif exp.op == "==" then return (eval_exp(defs, exp[1]) == eval_exp(defs, exp[2])) and 1 or 0
    elseif exp.op == "!=" then return (eval_exp(defs, exp[1]) ~= eval_exp(defs, exp[2])) and 1 or 0
    elseif exp.op == ">=" then return (eval_exp(defs, exp[1]) >= eval_exp(defs, exp[2])) and 1 or 0
    elseif exp.op == "<=" then return (eval_exp(defs, exp[1]) <= eval_exp(defs, exp[2])) and 1 or 0
    elseif exp.op == ">"  then return (eval_exp(defs, exp[1]) > eval_exp(defs, exp[2])) and 1 or 0
    elseif exp.op == "<"  then return (eval_exp(defs, exp[1]) < eval_exp(defs, exp[2])) and 1 or 0
    elseif exp.op == "!"  then return (eval_exp(defs, exp[1]) == 1) and 0 or 1
    elseif exp.op == "&&" then
        for _, e in ipairs(exp) do
            if eval_exp(defs, e) == 0 then
                return 0
            end
        end
        return 1
    elseif exp.op == "||" then
        for _, e in ipairs(exp) do
            if eval_exp(defs, e) ~= 0 then
                return 1
            end
        end
        return 0
    elseif exp.op == "?" then
        if eval_exp(defs, exp[1]) ~= 0 then
            return eval_exp(defs, exp[2])
        else
            return eval_exp(defs, exp[3])
        end
    elseif defs and exp.op == "defined" then
        return (defs[exp[1][1]] ~= nil) and 1 or 0
    else
        error("unimplemented operator " .. tostring(exp.op))
    end
end)

exps.run_expression = typed("Ctx, {string} -> boolean", function(ctx, tks)
    local exp = parse_expression(tks)
    return eval_exp(ctx.defines, exp) ~= 0
end)

exps.eval_parsed_expression = typed("table -> number", function(exp)
    return eval_exp(nil, exp)
end)

return exps
