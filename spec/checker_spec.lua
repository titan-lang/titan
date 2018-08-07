local checker = require 'titan-compiler.checker'
local parser = require 'titan-compiler.parser'
local types = require 'titan-compiler.types'
local driver = require 'titan-compiler.driver'
local util = require 'titan-compiler.util'

local function run_checker(code)
    types.registry = {}
    driver.imported = {}
    local ast = assert(parser.parse("(checker_spec)", code))
    local t, errs = checker.check("test", ast, code, "test.titan", driver.defaultloader())
    return #errs == 0, table.concat(errs, "\n"), ast, t
end

local function run_checker_modules(modules, main)
    local imported = {}
    local loader = driver.tableloader(modules, imported)
    local modt, err = checker.checkimport(main, loader)
    if not modt then return false, err end
    return #err == 0, table.concat(err, "\n"), imported
end

-- Return a version of t2 that only contains fields present in t1 (recursively)
-- Example:
--   t1  = { b = { c = 10 } e = 40 }
--   t2  = { a = 1, b = { c = 20, d = 30} }
--   out = { b = { c = 20 } }
local function restrict(t1, t2)
    if type(t1) == 'table' and type(t2) == 'table' then
        local out = {}
        for k,_ in pairs(t1) do
            out[k] = restrict(t1[k], t2[k])
        end
        return out
    else
        return t2
    end
end

local function assert_type_check(code)
    local ok, err = run_checker(code)
    assert.truthy(ok, err)
end

local function assert_type_error(expected, code)
    local ok, err = run_checker(code)
    assert.falsy(ok)
    assert.match(expected, err)
end

-- To avoid having these tests break all the time when we make insignificant
-- changes to the AST, we only verify a subset of the AST.
local function assert_ast(program, expected)
    local received = restrict(expected, program)
    assert.are.same(expected, received)
end

describe("Titan type checker", function()
    it("for loop iteration variables don't shadow var limit and step", function()
        local code = [[
            function fn(x: integer): integer
                local i: string = "asdfg"
                for i = 1, #i do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("detects invalid types", function()
        local code = [[
            function fn(): foo
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("invalid type 'test%.foo'", err)
    end)

    it("coerces to integer", function()
        local code = [[
            function fn(): integer
                local f: float = 1.0
                local i: integer = f
                return 1
            end
        ]]
        local ok, err, ast = run_checker(code)
        assert.truthy(ok)
        assert.same("Ast.ExpCast", ast[1].block.stats[2].exps[1]._tag)
        assert.same("Type.Integer", ast[1].block.stats[2].exps[1].target._tag)
    end)

    it("coerces to float", function()
        local code = [[
            function fn(): integer
                local i: integer = 12
                local f: float = i
                return 1
            end
        ]]
        local ok, err, ast = run_checker(code)
        assert.truthy(ok)
        assert.same("Ast.ExpCast", ast[1].block.stats[2].exps[1]._tag)
        assert.same("Type.Float", ast[1].block.stats[2].exps[1].target._tag)
    end)

    it("catches duplicate function declarations", function()
        local code = [[
            function fn(): integer
                return 1
            end
            function fn(): integer
                return 1
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("duplicate declaration", err)
    end)

    it("catches duplicate variable declarations", function()
        local code = {[[
            local x = 1
            x = 2
        ]],
        [[
            local x: integer = 1
            x = 2
        ]],
        }
        for _, c in ipairs(code) do
            local ok, err = run_checker(c)
            assert.falsy(ok)
            assert.match("duplicate declaration", err)
        end
    end)


    it("allows constant variable initialization", function()
        assert_type_check([[ x1 = nil ]])
        assert_type_check([[ x2 = false ]])
        assert_type_check([[ x3 = 11 ]])
        assert_type_check([[ x4 = 1.1 ]])
        assert_type_check([[ x5 = "11" ]])
        assert_type_check([[ x6 = {} ]])
        assert_type_check([[ x7 = {1, 2} ]])
        assert_type_check([[ x8 = "a" .. 10 ]])
        assert_type_check([[ x9 = 1 + 2 ]])
        assert_type_check([[ x10 = not false ]])
        assert_type_check([[ x11: integer = 10.1 ]])
    end)

    it("catches non constant variable initialization in top level", function()
        local assert_const = util.curry(assert_type_error, "must be constant")
        assert_const([[ function f(): integer return 10 end x = f() ]])
        assert_const([[ x = 10 y = x ]])
        assert_const([[ x = 10 y = -x ]])
        assert_const([[ x = 10 y = 10 + x ]])
        assert_const([[ x = 10 y = "a" .. x ]])
        assert_const([[ x = 10 y: float = x ]])
        assert_const([[ x = 10 y = {x} ]])
        assert_const([[ x = ({1})[2] ]])
    end)

    it("catches variable not declared", function()
        local code = [[
            function fn()
                local x:integer = 1
                y = 2
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("variable '%w+' not declared", err)
    end)

    it("reads from array", function()
        assert_type_check([[
            function fn(): integer
                local arr: {integer} = {}
                local x = arr[1]
                local y? = arr[1]
                local z: integer = arr[1]
                local w: float = arr[1]
                return x + y
            end
        ]])
    end)

    it("catches array expression in indexing is not an array or map", function()
        local code = [[
            function fn(x: integer)
                x[1] = 2
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expression in indexing is not an array or map", err)
    end)

    it("accepts correct use of length operator", function()
        local code = [[
            function fn(x: {integer}): integer
                return #x
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("catches wrong use of length operator", function()
        local code = [[
            function fn(x: integer): integer
                return #x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("trying to take the length", err)
    end)

    it("catches wrong use of length operator in maps", function()
        local code = [[
            function fn(x: {integer: integer}): integer
                return #x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("trying to take the length", err)
    end)

    it("catches wrong use of unary minus", function()
        local code = [[
            function fn(x: boolean): boolean
                return -x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("trying to negate a", err)
    end)

    it("catches wrong use of bitwise not", function()
        local code = [[
            function fn(x: boolean): boolean
                return ~x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("trying to bitwise negate a", err)
    end)

    it("catches mismatching types in locals", function()
        local code = [[
            function fn()
                local i: integer = 1
                local s: string = "foo"
                s = i
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected string but found integer", err)
    end)

    it("function can call another function", function()
        local code = [[
            function fn1()
              fn2()
            end

            function fn2()
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("catches mismatching types in arguments", function()
        local code = [[
            function fn(i: integer, s: string): integer
                s = i
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected string but found integer", err)
    end)

    it("allows setting element of array as nil", function ()
        local code = [[
            function fn()
                local arr: {integer} = { 10, 20, 30 }
                arr[1] = nil
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok, err)
    end)

    it("allows setting element of map as nil", function ()
        local code = [[
            function fn()
                local m: {string: integer} = { ["a"] = 10, ["b"] = 20, ["c"] = 30 }
                m["a"] = nil
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok, err)
    end)

    it("coerces map key", function ()
        assert_type_check([[
            function fn()
                local a: { float: string } = {}
                local s = a[1]
                a[1] = s
            end
        ]])
    end)

    it("typechecks multiple return values in array initialization", function ()
        local code = [[
            function f(): (integer, integer)
                return 10, 20
            end
            function g(): (integer, string)
                return 20, "foo"
            end
            function fn()
                local arr: {integer} = { 10, g(), 30, f() }
                local arr: {integer} = { 10, g(), 30, (g()) }
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok, err)
    end)

    it("typechecks multiple return values in map initialization", function ()
        local code = [[
            function f(): (integer, integer)
                return 10, 20
            end
            function g(): (integer, string)
                return 20, "foo"
            end
            function fn()
                local m1: {string: integer} = { ["a"] = 10, ["b"] = g(), ["c"] = 30, ["d"] = f() }
                local m2: {string: integer} = { ["a"] = 10, ["b"] = g(), ["c"] = 30, ["d"] = (g()) }
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok, err)
    end)

    it("catches wrong type in multiple return values for array initialization", function ()
        local code = [[
            function g(): (integer, string)
                return 20, "foo"
            end
            function fn()
                local arr: {integer} = { 10, g() }
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected integer but found string", err)
    end)

    it("drops extra values in multiple return values for map initialization", function ()
        local code = [[
            function g(): (integer, string)
                return 20, "foo"
            end
            function fn()
                local m: {string: integer} = { ["a"] = 10, ["b"] = g() }
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok, err)
    end)

    it("catches wrong type in multiple return values for multiple assignment", function ()
        local code = [[
            function f(x: integer): (integer, string)
                return x * 2, "foo"
            end

            function g(): string
                local x, y = 0, ""
                x, y = f(2)
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected string but found integer", err)
    end)

    it("catches named init list assigned to an array", function()
        local code = [[
            function fn(x: integer)
                local arr: {integer} = { x = 10 }
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("initializing field 'x' when expecting array", err)
    end)

    it("catches named init list assigned to a map", function()
        local code = [[
            function fn(x: integer)
                local m: {integer: string} = { x = 10 }
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("initializing field 'x' when expecting map", err)
    end)

    -- FIXME should this be allowed?
    it("catches named init list assigned to a string-keyed map", function()
        local code = [[
            function fn(x: integer)
                local m: {string: integer} = { x = 10 }
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("initializing field 'x' when expecting map", err)
    end)

    it("type-checks numeric 'for' (integer, implicit step)", function()
        local code = [[
            function fn(x: integer): integer
                for i:integer = 1, 10 do
                    x = x + 1
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("type-checks numeric 'for' (integer, explicit step)", function()
        local code = [[
            function fn(x: integer): integer
                for i:integer = 1, 10, 2 do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("type-checks numeric 'for' (float, implicit step)", function()
        local code = [[
            function fn(x: float): float
                for i:float = 1.0, 10.0 do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("type-checks numeric 'for' (float, explicit step)", function()
        local code = [[
            function fn(x: float): float
                for i:float = 1.0, 10.0, 2.0 do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("type-checks 'while'", function()
        local code = [[
            function fn(x: integer): integer
                local i: integer = 15
                while x < 100 do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("type-checks 'if'", function()
        local code = [[
            function fn(x: integer): integer
                local i: integer = 15
                if x < 100 then
                    x = x + i
                elseif x > 100 then
                    x = x - i
                else
                    x = 100
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("checks code inside the 'while' black", function()
        local code = [[
            function fn(x: integer): integer
                local i: integer = 15
                while i do
                    local s: string = i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected string but found integer", err)
    end)

    it("ensures numeric 'for' variable has number type (with annotation)", function()
        local code = [[
            function fn(x: integer, s: string): integer
                for i: string = 1, 10, 2 do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("control variable", err)
    end)

    it("ensures numeric 'for' variable has number type (without annotation)", function()
        local code = [[
            function fn(x: integer, s: string): integer
                for i = s, 10, 2 do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("control variable", err)
    end)


    it("catches 'for' errors in the start expression", function()
        local code = [[
            function fn(x: integer, s: string): integer
                for i:integer = s, 10, 2 do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("'for' start expression", err)
    end)


    it("catches 'for' errors in the finish expression", function()
        local code = [[
            function fn(x: integer, s: string): integer
                for i = 1, s, 2 do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("'for' finish expression", err)
    end)

    it("catches 'for' errors in the step expression", function()
        local code = [[
            function fn(x: integer, s: string): integer
                for i = 1, 10, s do
                    x = x + i
                end
                return x
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("'for' step expression", err)
    end)

    it("detects nil returns on non-nil functions", function()
        local code = {[[
            function fn(): integer
            end
        ]],
        [[
            function getval(a:integer): integer
                if a == 1 then
                    return 10
                elseif a == 2 then
                else
                    return 30
                end
            end
        ]],
        [[
            function getval(a:integer): integer
                if a == 1 then
                    return 10
                elseif a == 2 then
                    return 20
                else
                    if a < 5 then
                        if a == 3 then
                            return 30
                        end
                    else
                        return 50
                    end
                end
            end
        ]],
        }
        for _, c in ipairs(code) do
            local ok, err = run_checker(c)
            assert.falsy(ok)
            assert.match("function can return nil", err)
        end
    end)

    it("checks return type against declared type for one return value", function()
        local code = [[
            function fn(): integer
                return "foo"
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("types in return do not match", err)
    end)

    it("checks returning less values than function expects", function()
        local code = [[
            function fn(): (integer, string)
                return 20
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("returned 1 value%(s%) but function expected 2", err)
    end)

    it("checks returning more values than function expects", function()
        local code = [[
            function fn(): (integer, string)
                return 20, "foo", true
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("returned 3 value%(s%) but function expected 2", err)
    end)

    it("checks that returned values correctly match signature", function()
        local code = [[
            function fn(): (integer, string, boolean)
                return 20, "foo", true
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    it("checks that returned values incorrectly match signature", function()
        local code = [[
            function fn(): (integer, string, string)
                return "foo", true, 20
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected integer but found string", err)
        assert.match("expected string but found boolean", err)
        assert.match("expected string but found integer", err)
    end)

    it("detects attempts to call non-functions", function()
        local code = [[
            function fn(): integer
                local i: integer = 0
                i()
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("is not a function", err)
    end)

    for _, op in ipairs({"==", "~=", "<", ">", "<=", ">="}) do
        it("coerces "..op.." to float if any side is a float", function()
            local code = [[
                function fn(): integer
                    local i: integer = 1
                    local f: float = 1.5
                    local i_f = i ]] .. op .. [[ f
                    local f_i = f ]] .. op .. [[ i
                    local f_f = f ]] .. op .. [[ f
                    local i_i = i ]] .. op .. [[ i
                end
            ]]
            local ok, err, ast = run_checker(code)

            assert.same(types.Float(), ast[1].block.stats[3].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[3].exps[1].rhs._type)
            assert.same(types.Boolean(), ast[1].block.stats[3].exps[1]._type)

            assert.same(types.Float(), ast[1].block.stats[4].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[4].exps[1].rhs._type)
            assert.same(types.Boolean(), ast[1].block.stats[4].exps[1]._type)

            assert.same(types.Float(), ast[1].block.stats[5].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[5].exps[1].rhs._type)
            assert.same(types.Boolean(), ast[1].block.stats[5].exps[1]._type)

            assert.same(types.Integer(), ast[1].block.stats[6].exps[1].lhs._type)
            assert.same(types.Integer(), ast[1].block.stats[6].exps[1].rhs._type)
            assert.same(types.Boolean(), ast[1].block.stats[6].exps[1]._type)
        end)
    end

    for _, op in ipairs({"+", "-", "*", "%", "//"}) do
        it("coerces "..op.." to float if any side is a float", function()
            local code = [[
                function fn(): integer
                    local i: integer = 1
                    local f: float = 1.5
                    local i_f = i ]] .. op .. [[ f
                    local f_i = f ]] .. op .. [[ i
                    local f_f = f ]] .. op .. [[ f
                    local i_i = i ]] .. op .. [[ i
                end
            ]]
            local ok, err, ast = run_checker(code)

            assert.same(types.Float(), ast[1].block.stats[3].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[3].exps[1].rhs._type)
            assert.same(types.Float(), ast[1].block.stats[3].exps[1]._type)

            assert.same(types.Float(), ast[1].block.stats[4].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[4].exps[1].rhs._type)
            assert.same(types.Float(), ast[1].block.stats[4].exps[1]._type)

            assert.same(types.Float(), ast[1].block.stats[5].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[5].exps[1].rhs._type)
            assert.same(types.Float(), ast[1].block.stats[5].exps[1]._type)

            assert.same(types.Integer(), ast[1].block.stats[6].exps[1].lhs._type)
            assert.same(types.Integer(), ast[1].block.stats[6].exps[1].rhs._type)
            assert.same(types.Integer(), ast[1].block.stats[6].exps[1]._type)
        end)
    end

    it("typechecks and", function ()
        local code = [[
            function fn(): integer
                local i: integer = 1
                local f: float = 1.5
                local b: boolean = true
                local i_f = i and f
                local f_i = f and i
                local f_f = f and f
                local i_i = i and i
                local b_f = b and f
                local f_b = f and b
                local n = nil and f
                local of: float? = 2.0
                local b_of = b and of
            end
        ]]
        local ok, err, ast = run_checker(code)

        assert.same(types.Float(), ast[1].block.stats[4].exps[1]._type)
        assert.same(types.Integer(), ast[1].block.stats[5].exps[1]._type)
        assert.same(types.Float(), ast[1].block.stats[6].exps[1]._type)
        assert.same(types.Integer(), ast[1].block.stats[7].exps[1]._type)
        assert.same(types.Option(types.Float()), ast[1].block.stats[8].exps[1].exp._type)
        assert.same(types.Boolean(), ast[1].block.stats[9].exps[1]._type)
        assert.same(types.Nil(), ast[1].block.stats[10].exps[1]._type)
        assert.same(types.Option(types.Float()), ast[1].block.stats[12].exps[1].exp._type)
    end)

    it("typechecks or", function ()
        local code = [[
            function fn(): integer
                local i: integer = 1
                local f: float = 1.5
                local b: boolean = true
                local f_i = f or i
                local i_f = i or f
                local f_f = f or f
                local i_i = i or i
                local b_f: boolean = b or f
                local f_b: boolean = f or b
                local n = nil or f
                local of: float? = 2.0
                local of_f = of or f
                local v: value = 1
                local of_v = of or v
                local v_f = v or f
            end
        ]]
        local ok, err, ast = run_checker(code)

        assert.same(types.Float(), ast[1].block.stats[4].exps[1]._type)
        assert.same(types.Integer(), ast[1].block.stats[5].exps[1]._type)
        assert.same(types.Float(), ast[1].block.stats[6].exps[1]._type)
        assert.same(types.Integer(), ast[1].block.stats[7].exps[1]._type)
        assert.same(types.Boolean(), ast[1].block.stats[8].exps[1]._type)
        assert.same(types.Boolean(), ast[1].block.stats[9].exps[1]._type)
        assert.same(types.Float(), ast[1].block.stats[10].exps[1]._type)
        assert.same(types.Float(), ast[1].block.stats[12].exps[1]._type)
        assert.same(types.Value(), ast[1].block.stats[14].exps[1]._type)
        assert.same(types.Value(), ast[1].block.stats[15].exps[1]._type)
    end)

    for _, op in ipairs({"|", "&", "<<", ">>"}) do
        it("coerces "..op.." to integer if other side is a float", function()
            local code = [[
                function fn(): integer
                    local i: integer = 1
                    local f: float = 1.5
                    local i_f = i ]] .. op .. [[ f
                    local f_i = f ]] .. op .. [[ i
                    local f_f = f ]] .. op .. [[ f
                    local i_i = i ]] .. op .. [[ i
                end
            ]]
            local ok, err, ast = run_checker(code)

            assert.same(types.Integer(), ast[1].block.stats[3].exps[1].lhs._type)
            assert.same(types.Integer(), ast[1].block.stats[3].exps[1].rhs._type)
            assert.same(types.Integer(), ast[1].block.stats[3].exps[1]._type)

            assert.same(types.Integer(), ast[1].block.stats[4].exps[1].lhs._type)
            assert.same(types.Integer(), ast[1].block.stats[4].exps[1].rhs._type)
            assert.same(types.Integer(), ast[1].block.stats[4].exps[1]._type)

            assert.same(types.Integer(), ast[1].block.stats[5].exps[1].lhs._type)
            assert.same(types.Integer(), ast[1].block.stats[5].exps[1].rhs._type)
            assert.same(types.Integer(), ast[1].block.stats[5].exps[1]._type)

            assert.same(types.Integer(), ast[1].block.stats[6].exps[1].lhs._type)
            assert.same(types.Integer(), ast[1].block.stats[6].exps[1].rhs._type)
            assert.same(types.Integer(), ast[1].block.stats[6].exps[1]._type)
        end)
    end

    for _, op in ipairs({"/", "^"}) do
        it("always coerces "..op.." to float", function()
            local code = [[
                function fn(): integer
                    local i: integer = 1
                    local f: float = 1.5
                    local i_f = i ]] .. op .. [[ f
                    local f_i = f ]] .. op .. [[ i
                    local f_f = f ]] .. op .. [[ f
                    local i_i = i ]] .. op .. [[ i
                end
            ]]
            local ok, err, ast = run_checker(code)

            assert.same(types.Float(), ast[1].block.stats[3].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[3].exps[1].rhs._type)
            assert.same(types.Float(), ast[1].block.stats[3].exps[1]._type)

            assert.same(types.Float(), ast[1].block.stats[4].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[4].exps[1].rhs._type)
            assert.same(types.Float(), ast[1].block.stats[4].exps[1]._type)

            assert.same(types.Float(), ast[1].block.stats[5].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[5].exps[1].rhs._type)
            assert.same(types.Float(), ast[1].block.stats[5].exps[1]._type)

            assert.same(types.Float(), ast[1].block.stats[6].exps[1].lhs._type)
            assert.same(types.Float(), ast[1].block.stats[6].exps[1].rhs._type)
            assert.same(types.Float(), ast[1].block.stats[6].exps[1]._type)
        end)
    end

    for _, op in ipairs({"+", "-", "*", "%", "//", "/", "^"}) do
        it("fails if one side of expression is value", function ()
            local code = [[
                function fn(): integer
                    local i: value = 1
                    local f: float = 1.5
                    local i_f = i ]] .. op .. [[ f
                    local f_i = f ]] .. op .. [[ i
                end
            ]]
            local ok, err, ast = run_checker(code)
            assert.falsy(ok)
            assert.match("is a value instead of a number", err)
        end)
    end

    it("cannot concatenate with boolean", function()
        local code = [[
            function fn()
                local s = "foo" .. true
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("cannot concatenate with boolean value", err)
    end)

    it("cannot concatenate with nil", function()
        local code = [[
            function fn()
                local s = "foo" .. nil
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("cannot concatenate with nil value", err)
    end)

    it("cannot concatenate with array", function()
        local code = [[
            function fn()
                local s = "foo" .. {}
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("cannot concatenate with { integer } value", err)
    end)

    it("cannot concatenate with map", function()
        local code = [[
            function fn()
                local s = "foo" .. { [false] = 2 }
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("cannot concatenate with { boolean : integer } value", err)
    end)

    it("cannot concatenate with type value", function()
        local code = [[
            function fn()
                local v: value = "bar"
                local s = "foo" .. v
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("cannot concatenate with value", err)
    end)

    it("can concatenate with integer and float", function()
        local code = [[
            function fn()
                local s = 1 .. 2.5
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
    end)

    for _, op in ipairs({"==", "~="}) do
        it("can compare arrays of same type using " .. op, function()
            local code = [[
                function fn(a1: {integer}, a2: {integer}): boolean
                    return a1 ]] .. op .. [[ a2
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)

        it("can compare maps of same type using " .. op, function()
            local code = [[
                function fn(a1: {string: integer}, a2: {string: integer}): boolean
                    return a1 ]] .. op .. [[ a2
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)
    end

    for _, op in ipairs({"==", "~="}) do
        it("can compare booleans using " .. op, function()
            local code = [[
                function fn(b1: string, b2: string): boolean
                    return b1 ]] .. op .. [[ b2
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)
    end

    for _, op in ipairs({"==", "~="}) do
        it("can compare pointers using " .. op, function()
            local code = [[
                local stdio = foreign import "stdio.h"
                function fn(): boolean
                    local f1 = stdio.fopen("a.txt", "w")
                    local f2 = stdio.fopen("a.txt", "w")
                    return f1 ]] .. op .. [[ f2
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok, err)
        end)
    end

    for _, op in ipairs({"==", "~="}) do
        it("can compare pointers to nil using " .. op, function()
            local code = [[
                local stdio = foreign import "stdio.h"
                function fn(): boolean
                    local f1 = stdio.fopen("a.txt", "w")
                    return f1 ]] .. op .. [[ nil
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)
    end

    for _, op in ipairs({"<", ">", "<=", ">="}) do
        it("cannot compare pointers using " .. op, function()
            local code = [[
                local stdio = foreign import "stdio.h"
                function fn(): boolean
                    local f1 = stdio.fopen("a.txt", "w")
                    local f2 = stdio.fopen("a.txt", "w")
                    return f1 ]] .. op .. [[ f2
                end
            ]]
            local ok, err = run_checker(code)
            assert.falsy(ok)
            assert.match("trying to use relational expression", err)
        end)
    end

    for _, op in ipairs({"==", "~=", "<", ">", "<=", ">="}) do
        it("can compare floats using " .. op, function()
            local code = [[
                function fn(f1: string, f2: string): boolean
                    return f1 ]] .. op .. [[ f2
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)
    end

    for _, op in ipairs({"==", "~=", "<", ">", "<=", ">="}) do
        it("can compare integers using " .. op, function()
            local code = [[
                function fn(i1: string, i2: string): boolean
                    return i1 ]] .. op .. [[ i2
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)
    end

    for _, op in ipairs({"==", "~=", "<", ">", "<=", ">="}) do
        it("can compare integers and floats using " .. op, function()
            local code = [[
                function fn(i: integer, f: float): boolean
                    return i ]] .. op .. [[ f
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)
    end

    for _, op in ipairs({"==", "~=", "<", ">", "<=", ">="}) do
        it("can compare strings using " .. op, function()
            local code = [[
                function fn(s1: string, s2: string): boolean
                    return s1 ]] .. op .. [[ s2
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)
    end

    for _, op in ipairs({"==", "~="}) do
        it("cannot compare arrays of different types using " .. op, function()
            local code = [[
                function fn(a1: {integer}, a2: {float}): boolean
                    return a1 ]] .. op .. [[ a2
                end
            ]]
            local ok, err = run_checker(code)
            assert.falsy(ok)
            assert.match("trying to compare values of incomparable types", err)
        end)

        it("cannot compare maps of different value types using " .. op, function()
            local code = [[
                function fn(a1: {string: integer}, a2: {string: float}): boolean
                    return a1 ]] .. op .. [[ a2
                end
            ]]
            local ok, err = run_checker(code)
            assert.falsy(ok)
            assert.match("trying to compare values of incomparable types", err)
        end)

        it("cannot compare maps of different key types using " .. op, function()
            local code = [[
                function fn(a1: {integer: string}, a2: {float: string}): boolean
                    return a1 ]] .. op .. [[ a2
                end
            ]]
            local ok, err = run_checker(code)
            assert.falsy(ok)
            assert.match("trying to compare values of incomparable types", err)
        end)
    end

    for _, op in ipairs({"==", "~="}) do
        for _, t1 in ipairs({"{integer}", "boolean", "float", "string"}) do
            for _, t2 in ipairs({"{integer}", "boolean", "float", "string"}) do
                if t1 ~= t2 then
                    it("cannot compare " .. t1 .. " and " .. t2 .. " using " .. op, function()
                        local code = [[
                            function fn(a: ]] .. t1 .. [[, b: ]] .. t2 .. [[): boolean
                                return a ]] .. op .. [[ b
                            end
                        ]]
                        local ok, err = run_checker(code)
                        assert.falsy(ok)
                        assert.match("trying to compare values of incomparable types", err)
                    end)
                end
            end
        end
    end

    for _, op in ipairs({"<", ">", "<=", ">="}) do
        for _, t in ipairs({"{integer}", "boolean", "string"}) do
            it("cannot compare " .. t .. " and float using " .. op, function()
                local code = [[
                    function fn(a: ]] .. t .. [[, b: float): boolean
                        return a ]] .. op .. [[ b
                    end
                ]]
                local ok, err = run_checker(code)
                assert.falsy(ok)
                assert.match("left hand side of relational expression is", err)
            end)
        end
    end

    for _, op in ipairs({"<", ">", "<=", ">="}) do
        for _, t in ipairs({"{integer}", "boolean", "string"}) do
            it("cannot compare float and " .. t .. " using " .. op, function()
                local code = [[
                    function fn(a: float, b: ]] .. t .. [[): boolean
                        return a ]] .. op .. [[ b
                    end
                ]]
                local ok, err = run_checker(code)
                assert.falsy(ok)
                assert.match("right hand side of relational expression is", err)
            end)
        end
    end

    for _, op in ipairs({"<", ">", "<=", ">="}) do
        for _, t in ipairs({"{integer}", "boolean", "string"}) do
            it("cannot compare " .. t .. " and integer using " .. op, function()
                local code = [[
                    function fn(a: ]] .. t .. [[, b: integer): boolean
                        return a ]] .. op .. [[ b
                    end
                ]]
                local ok, err = run_checker(code)
                assert.falsy(ok)
                assert.match("left hand side of relational expression is", err)
            end)
        end
    end

    for _, op in ipairs({"<", ">", "<=", ">="}) do
        for _, t in ipairs({"{integer}", "boolean", "string"}) do
            it("cannot compare integer and " .. t .. " using " .. op, function()
                local code = [[
                    function fn(a: integer, b: ]] .. t .. [[): boolean
                        return a ]] .. op .. [[ b
                    end
                ]]
                local ok, err = run_checker(code)
                assert.falsy(ok)
                assert.match("right hand side of relational expression is", err)
            end)
        end
    end

    for _, op in ipairs({"<", ">", "<=", ">="}) do
        for _, t in ipairs({"{integer}", "boolean"}) do
            it("cannot compare " .. t .. " and string using " .. op, function()
                local code = [[
                    function fn(a: ]] .. t .. [[, b: string): boolean
                        return a ]] .. op .. [[ b
                    end
                ]]
                local ok, err = run_checker(code)
                assert.falsy(ok)
                assert.match("left hand side of relational expression is", err)
            end)
        end
    end

    for _, op in ipairs({"<", ">", "<=", ">="}) do
        for _, t in ipairs({"{integer}", "boolean"}) do
            it("cannot compare string and " .. t .. " using " .. op, function()
                local code = [[
                    function fn(a: string, b: ]] .. t .. [[): boolean
                        return a ]] .. op .. [[ b
                    end
                ]]
                local ok, err = run_checker(code)
                assert.falsy(ok)
                assert.match("right hand side of relational expression is", err)
            end)
        end
    end

    for _, op in ipairs({"<", ">", "<=", ">="}) do
        for _, t1 in ipairs({"{integer}", "boolean"}) do
            for _, t2 in ipairs({"{integer}", "boolean"}) do
                it("cannot compare " .. t1 .. " and " .. t2 .. " using " .. op, function()
                    local code = [[
                        function fn(a: ]] .. t1 .. [[, b: ]] .. t2 .. [[): boolean
                            return a ]] .. op .. [[ b
                        end
                    ]]
                    local ok, err = run_checker(code)
                    assert.falsy(ok)
                    if t1 ~= t2 then
                        assert.match("trying to use relational expression with", err)
                    else
                        assert.match("trying to use relational expression with two", err)
                    end
                end)
            end
        end
    end

    for _, t1 in ipairs({"{integer}", "integer", "string"}) do
        for _, t2 in ipairs({"integer", "integer", "string"}) do
            if t1 ~= t2 then
                it("cannot evaluate " .. t1 .. " and " .. t2 .. " using 'or'", function()
                    local code = [[
                        function fn(a: ]] .. t1 .. [[?, b: ]] .. t2 .. [[): ]] .. t2 .. [[
                            return a or b
                        end
                    ]]
                    local ok, err = run_checker(code)
                    assert.falsy(ok)
                    assert.match("left hand side of 'or' is a", err)
                end)
            end
        end
    end

    for _, t1 in ipairs({"{integer}", "integer", "string"}) do
        for _, t2 in ipairs({"integer", "integer", "string"}) do
            if t1 ~= t2 then
                it("typechecks " .. t1 .. " and " .. t2 .. " using 'or' in boolean context", function()
                    local code = [[
                        function fn(a: ]] .. t1 .. [[, b: ]] .. t2 .. [[): boolean
                            return a or b
                        end
                    ]]
                    local ok, err = run_checker(code)
                    assert.truthy(ok, err)
                end)
            end
        end
    end

    for _, t1 in ipairs({"{integer}", "integer", "string"}) do
        for _, t2 in ipairs({"integer", "integer", "string"}) do
            if t1 ~= t2 then
                it("typechecks " .. t1 .. " and " .. t2 .. " using 'and'", function()
                    local code = [[
                        function fn(a: ]] .. t1 .. [[, b: ]] .. t2 .. [[): boolean
                            return a and b
                        end
                    ]]
                    local ok, err = run_checker(code)
                    assert.truthy(ok, err)
                end)
            end
        end
    end

    for _, op in ipairs({"|", "&", "<<", ">>"}) do
        it("can use bitwise operators with integers using " .. op, function()
            local code = [[
                function fn(i1: integer, i2: integer): integer
                    return i1 ]] .. op .. [[ i2
                end
            ]]
            local ok, err = run_checker(code)
            assert.truthy(ok)
        end)
    end

    for _, op in ipairs({"|", "&", "<<", ">>"}) do
        for _, t in ipairs({"{integer}", "boolean", "string"}) do
            it("cannot use bitwise operator " .. op .. " when left hand side is not integer", function()
                local code = [[
                    function fn(a: ]] .. t .. [[, b: integer): boolean
                        return a ]] .. op .. [[ b
                    end
                ]]
                local ok, err = run_checker(code)
                assert.falsy(ok)
                assert.match("left hand side of arithmetic expression is a", err)
            end)
        end
    end

    for _, op in ipairs({"|", "&", "<<", ">>"}) do
        for _, t in ipairs({"{integer}", "boolean", "string"}) do
            it("cannot use bitwise operator " .. op .. " when right hand side is not integer", function()
                local code = [[
                    function fn(a: integer, b: ]] .. t .. [[): boolean
                        return a ]] .. op .. [[ b
                    end
                ]]
                local ok, err = run_checker(code)
                assert.falsy(ok)
                assert.match("right hand side of arithmetic expression is a", err)
            end)
        end
    end

    for _, t in ipairs({"{integer}", "boolean", "float", "integer", "nil", "string", "Record"}) do
        it("can implicitly cast from value to " .. t, function()
            assert_type_check([[
                record Record end
                function fn(a: value): ]] .. t .. [[
                    return a
                end
            ]])
        end)
    end

    for _, t in ipairs({"{integer}", "boolean", "float", "integer", "nil", "string", "Record"}) do
        it("can explicitly cast from value to " .. t, function()
            assert_type_check([[
                record Record end
                function fn(a: value): ]] .. t .. [[
                    return a as ]] .. t .. [[
                end
            ]])
        end)
    end

    for _, t in ipairs({"{integer}", "boolean", "float", "integer", "nil", "string", "Record"}) do
        it("can explicitly cast from " .. t .. "to value", function()
            assert_type_check([[
                record Record end
                function fn(a: ]] .. t .. [[): value
                    return a as value
                end
            ]])
        end)
    end

    for _, t in ipairs({"{integer}", "boolean", "float", "integer", "nil", "string", "Record"}) do
        it("can implicitly cast from " .. t .. "to value", function()
            assert_type_check([[
                record Record end
                function fn(a: ]] .. t .. [[): value
                    return a
                end
            ]])
        end)
    end

    for _, t in ipairs({"boolean", "float", "integer", "nil", "string", "Record"}) do
        it("cannot explicitly cast from " .. t .. " to {integer}", function()
            assert_type_error("cannot cast", [[
                record Record end
                function fn(a: ]] .. t .. [[): {integer}
                    return a as {integer}
                end
            ]])
        end)
    end

    for _, t in ipairs({"{integer}", "boolean", "integer", "nil", "string", "Record"}) do
        it("cannot explicitly cast from " .. t .. " to float", function()
            assert_type_error("cannot cast", [[
                record Record end
                function fn(a: ]] .. t .. [[): float
                    return a as float
                end
            ]])
        end)
    end

    for _, t in ipairs({"{integer}", "boolean", "nil", "string", "Record"}) do
        it("cannot explicitly cast from " .. t .. " to integer", function()
            assert_type_error("cannot cast", [[
                record Record end
                function fn(a: ]] .. t .. [[): integer
                    return a as integer
                end
            ]])
        end)
    end

    for _, t in ipairs({"{integer}", "boolean", "float", "integer", "string", "Record"}) do
        it("cannot explicitly cast from " .. t .. " to nil", function()
            assert_type_error("cannot cast", [[
                record Record end
                function fn(a: ]] .. t .. [[): nil
                    return a as nil
                end
            ]])
        end)
    end

    for _, t in ipairs({"{integer}", "boolean", "nil", "Record"}) do
        it("cannot explicitly cast from " .. t .. " to string", function()
            assert_type_error("cannot cast", [[
                record Record end
                function fn(a: ]] .. t .. [[): string
                    return a as string
                end
            ]])
        end)
    end

    it("returns the type of the module with exported members", function()
        local modules = { test = [[
            a: integer = 1
            local b: float = 2
            function geta(): integer
                return a
            end
            local function foo() end
        ]] }
        local ok, err, mods = run_checker_modules(modules, "test")
        assert.truthy(ok)
        assert_ast(mods.test.type, {
            _tag = "Type.Module",
            name = "test",
            members = {
                a = { type = {_tag = "Type.Integer"} },
                geta = { type = {_tag = "Type.Function"} }
            }
        })
        assert.falsy(mods.test.type.members.b)
        assert.falsy(mods.test.type.members.foo)
    end)

    it("fails to load modules that do not exist", function ()
        local code = [[
            local foo = import "foo"
            local bar = import "bar.baz"
        ]]
        local ok, err, ast = run_checker(code)
        assert.falsy(ok)
        assert.match("module 'foo' not found", err)
        assert.match("module 'bar.baz' not found", err)
    end)

    it("fails to load foreign modules that do not exist", function ()
        local code = [[
            local foo = foreign import "does_not_exist.h"
            local bar = foreign import "not_a_header_file"
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("failed preprocessing 'does_not_exist.h'", err)
        assert.match("failed preprocessing 'not_a_header_file'", err)
    end)

    it("correctly imports modules that do exist", function ()
        local modules = {
            foo = [[
                a: integer = 1
                function foo() end
            ]],
            bar = [[
                local foo = import "foo"
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.truthy(ok)
        assert.truthy(mods.foo)
        assert_ast(mods.foo.type, {
            _tag = "Type.Module",
            name = "foo",
            members = {
                a = { type = {_tag = "Type.Integer"} },
                foo = { type = {_tag = "Type.Function"} }
            }
        })
    end)

    it("correctly imports foreign modules that do exist", function ()
        local code = [[
            local stdio = foreign import "stdio.h"
        ]]
        local ok, err, ast = run_checker(code)
        assert.truthy(ok)
        assert_ast(ast[1], {
            _tag = "Ast.TopLevelForeignImport",
            localname = "stdio",
            _type = {
                _tag = "Type.ForeignModule",
                members = {
                    printf = {
                        type = {_tag = "Type.Function"}
                    }
                }
            }
        })
    end)

    it("cannot convert different pointers without a cast", function()
        local code = [[
            local stdio = foreign import "stdio.h"
            local dirent = foreign import "dirent.h"
            function f()
                local dd = dirent.opendir(".")
                local n = stdio.fwrite("alo", 3, 1, dd)
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected pointer to FILE but found pointer to DIR", err)
    end)

    it("cannot convert void pointer to string without a cast", function()
        local code = [[
            local stdlib = foreign import "stdlib.h"
            local string_h = foreign import "string.h"
            function f()
                local mem = stdlib.realloc(nil, 100)
                local s = string_h.strcpy(mem, "Hello")
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected string but found void pointer", err)
    end)

    it("can convert void pointer to string with a cast", function()
        local code = [[
            local stdlib = foreign import "stdlib.h"
            local string_h = foreign import "string.h"
            function f()
                local mem = stdlib.realloc(nil, 100)
                local s = string_h.strcpy(mem as string, "Hello")
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok, err)
    end)

    it("can check foreign module variables", function()
        local code = [[
            local errno = foreign import "errno.h"
            function fun(name: string): integer
                return errno.errno
            end
        ]]
        local ok, err = run_checker(code)
        assert.truthy(ok)
        code = [[
            local errno = foreign import "errno.h"
            function fun(name: string): string
                return errno.errno
            end
        ]]
         ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("expected string but found integer", err)
    end)

    it("fails on circular module references", function ()
        local modules = {
            foo = [[
                local bar = import "bar"
                a: integer = nil
                function foo() end
            ]],
            bar = [[
                local foo = import "foo"
            ]]
        }
        local ok, err = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("circular", err)
    end)

    it("import fails on modules with syntax errors", function ()
        local modules = {
            foo = [[
                a: integer =
                function foo() end
            ]],
            bar = [[
                local foo = import "foo"
            ]]
        }
        local ok, err = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("problem loading module", err)
    end)

    it("correctly uses module variable", function ()
        local modules = {
            foo = [[
                a: integer = 1
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    foo.a = 5
                    return foo.a
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.truthy(ok)
    end)

    it("uses module variable with wrong type", function ()
        local modules = {
            foo = [[
                a: integer = 1
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): string
                    foo.a = "foo"
                    return foo.a
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("expected string but found integer", err)
        assert.match("expected integer but found string", err)
    end)

    it("catches module variable initialization with wrong type", function()
        local code = {[[
            local x: integer = nil
        ]],
        [[
            x: integer = nil
        ]],
        }
        for _, c in ipairs(code) do
            local ok, err = run_checker(c)
            assert.falsy(ok)
            assert.match("expected integer but found nil", err)
        end
    end)

    it("catches use of function as first-class value", function ()
        local code = [[
            function foo(): integer
                return foo
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("access a function", err)
    end)

    it("catches assignment to function", function ()
        local code = [[
            function foo(): integer
                foo = 2
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("assign to a function", err)
    end)

    it("catches use of external function as first-class value", function ()
        local modules = {
            foo = [[
                a: integer = 1
                function foo()
                end
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    return foo.foo
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("access a function", err)
    end)

    it("catches assignment to external function", function ()
        local modules = {
            foo = [[
                a: integer = 1
                function foo()
                end
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    foo.foo = 2
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("assign to a function", err)
    end)

    it("catches use of module as first-class value", function ()
        local modules = {
            foo = [[
                a: integer = 1
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    return foo
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("access module", err)
    end)

    it("catches assignment to module", function ()
        local modules = {
            foo = [[
                a: integer = 1
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    foo = 2
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("assign to a module", err)
    end)

    it("catches call of external non-function", function ()
        local modules = {
            foo = [[
                a: integer = 1
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    return foo.a()
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("'foo.a' is not a function", err)
    end)

    it("catches call of undeclared external function", function ()
        local modules = {
            foo = [[
                a: integer = 1
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    return foo.b()
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("member 'b' not found inside module 'foo'", err)
    end)

    it("catches call of non-function function", function ()
        local code = [[
            local a = 2
            function foo(): integer
                return a()
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match("'a' is not a function", err)
    end)

    it("correctly uses module function", function ()
        local modules = {
            foo = [[
                function a(): integer
                    return 42
                end
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    return foo.a()
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.truthy(ok)
    end)

    it("functions cannot have two parameters with the same name", function()
        local code = [[
            function f(a: integer, a: integer)
            end
        ]]
        local ok, err = run_checker(code)
        assert.falsy(ok)
        assert.match('duplicate parameter', err)
    end)

    it("catches local variable initialization with wrong type", function()
        local code = {[[
            function f()
                local x: integer = "foo"
            end
        ]], [[
            function f()
                local x: integer, y: integer = 20, "foo"
            end
        ]]}
        for _, c in ipairs(code) do
            local ok, err = run_checker(c)
            assert.falsy(ok)
            assert.match("expected integer but found string", err)
        end
    end)

    it("checks extra values on right hand side", function()
        assert_type_error("left%-hand side expects 1 value%(s%) but right%-hand side produces 3 value%(s%)", [[
            function f()
                local x: float = 10, 20, "foo"
                x = 10, 20, "foo"
            end
        ]])
    end)

    it("checks multiple declarations and assignment", function()
        assert_type_check([[
            function g(): (integer, string)
                return 20, "foo"
            end
            function f()
                local x: float, y: integer, z: string = 10, g()
                x, y, z = 10, g()
            end
        ]])
    end)

    it("checks that lhs and rhs arities match on declaration and assignment", function()
        assert_type_error("left%-hand side expects 2 value%(s%) but right%-hand side produces 3 value%(s%)", [[
            function g(): (integer, string)
                return 20, "foo"
            end
            function f()
                local x: float, y: integer = 10, g()
                x, y = 10, g()
            end
        ]])
    end)

    it("typechecks adjustment of argument lists", function()
        assert_type_check([[
            function g(): (integer, string)
                return 20, "foo"
            end
            function f(x: string, y: integer, z: string)
            end
            function h()
                f("bar", g())
            end
        ]])
    end)

    it("catches too few values on right hand side", function()
        local code = {[[
            function f()
                local x: integer, y: string, z: integer = 20, "foo"
            end
        ]],[[
            function g(): (integer, string)
                return 20, "foo"
            end
            function f()
                local x: integer, y: string, z: integer = g()
            end
        ]],[[
            function f()
                local x: integer = 0
                local y: string = ""
                local z: integer = 0
                x, y, z = 20, "foo"
            end
        ]],[[
            function g(): (integer, string)
                return 20, "foo"
            end
            function f()
                local x: integer = 0
                local y: string = ""
                local z: integer = 0
                x, y, z = g()
            end
        ]]}
        for _, c in ipairs(code) do
            local ok, err = run_checker(c)
            assert.falsy(ok)
            assert.match("left%-hand side expects 3 value%(s%) but right%-hand side produces 2 value%(s%)", err)
        end
    end)

    it("catches functions called with wrong arity", function()
        local cases = {{ code = [[
            function g(): (integer, integer)
                return 20, 30
            end
            function f(x: integer, y: integer, z: integer)
            end
            function h()
                f(g())
            end
        ]], args = 2, params = 3 }, { code = [[
            function g(): (integer, integer)
                return 20, 30
            end
            function f(x: integer, y: integer, z: integer)
            end
            function h()
                f(20, 30, g())
            end
        ]], args = 4, params = 3 }, { code = [[
            function g(): (integer, integer)
                return 20, 30
            end
            function f(x: integer, y: integer, z: integer)
            end
            function h()
                f(20, (g()))
            end
        ]], args = 2, params = 3 }}
        for _, c in ipairs(cases) do
            local ok, err = run_checker(c.code)
            assert.falsy(ok)
            assert.match("function 'f' called with " .. c.args .. " arguments but expects " .. c.params, err)
        end
    end)
end)

describe("Titan typecheck of records", function()
    it("typechecks record declarations", function()
        assert_type_check([[
            record Point
                x: float
                y: float
            end
        ]])
    end)

    it("detects type errors inside record declarations", function()
        assert_type_error("invalid type 'test%.notfound'", [[
            record Point
                x: notfound
            end
        ]])
    end)

    it("typechecks recursive record declarations", function()
        assert_type_check([[
            record List
                l: List
            end
        ]])
    end)

    it("typechecks mutually recursive record declarations", function()
        assert_type_check([[
            record A
                l: B
            end
            record B
                l: A
            end
        ]])
    end)

    it("typechecks record as argument/return", function()
        assert_type_check([[
            record Point x: float; y:float end

            function f(p: Point): Point
                return p
            end
        ]])
    end)

    it("typechecks record constructors", function()
        assert_type_check([[
            record Point x: float; y:float end

            p = Point.new(1, 2)
        ]])
        assert_type_error("no context", [[
            p = { foo = 2 }
        ]])
    end)

    it("doesn't typecheck invalid dot operation in record", function()
        assert_type_error("invalid record function 'nope'", [[
            record Point x: float; y:float end

            p = Point.nope(1, 2)
        ]])
    end)

    local function wrap_record(code)
        return [[
            record Point x: float; y:float end

            function f(p: Point): float
                ]].. code ..[[
            end
        ]]
    end

    it("doesn't typecheck constructor calls with wrong arguments", function()
        assert_type_error("expected float but found string",
                          wrap_record[[ p = Point.new("a", "b") ]])
        assert_type_error("'Point.new' called with 1 arguments but expects 2",
                          wrap_record[[ p = Point.new(1) ]])
        assert_type_error("'Point.new' called with 3 arguments but expects 2",
                          wrap_record[[ p = Point.new(1, 2, 3) ]])
    end)

    it("typechecks record read/write", function()
        assert_type_check(wrap_record[[
            local x: float = 10
            p.x = x
            return p.y
        ]])
    end)

    it("doesn't typecheck read/write to non existent fields", function()
        local function assert_non_existent(code)
            assert_type_error("field 'nope' not found in record 'test%.Point'",
                              wrap_record(code))
        end
        assert_non_existent([[ p.nope = 10 ]])
        assert_non_existent([[ return p.nope ]])
    end)

    it("doesn't typecheck duplicated field", function ()
        assert_type_error("redeclaration of field 'foo' in record 'Foo'",
                          [[
                              record Foo
                                foo: integer
                                foo: string
                              end
                          ]])
    end)

    it("doesn't typecheck method with same name as field", function ()
        assert_type_error("cannot declare method 'Foo:foo' as field 'foo' exists in record 'Foo'",
                          [[
                              record Foo
                                foo: integer
                              end

                              function Foo:foo()
                              end
                          ]])
    end)

    it("doesn't typecheck read/write with invalid types", function()
        assert_type_error("expected float but found test%.Point",
                          wrap_record[[ p.x = p ]])
        assert_type_error("expected test.%Point but found float",
                          wrap_record[[ local p: Point = p.x ]])
    end)

    it("doesn't typecheck method that does not reference a record", function()
        assert_type_error("record referenced by method declaration 'Point:foo' does not exist",
                          [[
                              function Point:foo()
                              end
                          ]])
        assert_type_error("member referenced by method declaration 'Point:foo' is not a record",
                          [[
                              Point = 20
                              function Point:foo()
                              end
                          ]])
    end)

    it("typechecks body of method declaration", function ()
        assert_type_error("expected string but found integer", [[
            record Foo end
            function Foo:foo(): string
                return 1
            end
        ]])
    end)

    it("binds self in method declaration", function ()
        assert_type_check([[
            record Point
                x: integer
                y: integer
            end
            function Point:move(dx: integer, dy: integer)
                self.x = self.x + dx
                self.y = self.y + dy
            end
        ]])
        assert_type_error("expected string but found test%.Point", [[
            record Point
                x: integer
                y: integer
            end
            function Point:foo(): string
                return self
            end
        ]])
    end)

    it("doesn't typecheck duplicate method declaration", function()
        assert_type_error("redeclaration of method 'Point:foo'",
                          [[
                              record Point end
                              function Point:foo()
                              end
                              function Point:foo()
                              end
                          ]])
    end)

    it("uses external record type wrongly", function ()
        local modules = {
            foo = [[
                record Point
                  x: float
                  y: float
                end
            ]],
            bar = [[
                local f = import "foo"
                function bar(p: f.Point): string
                    return p.x
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("expected string but found float", err)
    end)

    it("uses external record type correctly", function ()
        local modules = {
            foo = [[
                record Point
                  x: float
                  y: float
                end
            ]],
            bar = [[
                local f = import "foo"
                function bar(p: f.Point): float
                    return p.x
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.truthy(ok)
    end)

    it("uses transitive external record type wrongly", function ()
        local modules = {
            baz = [[
              local f = import "foo"
              function baz(): f.Point
                return f.Point.new(1,2)
              end
            ]],
            foo = [[
                record Point
                  x: float
                  y: float
                end
            ]],
            bar = [[
                local b = import "baz"
                function bar(): string
                    local p = b.baz()
                    return p.x
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("expected string but found float", err)
    end)

    it("uses transitive external record type correctly", function ()
        local modules = {
            baz = [[
              local f = import "foo"
              function baz(): f.Point
                return f.Point.new(1,2)
              end
            ]],
            foo = [[
                record Point
                  x: float
                  y: float
                end
            ]],
            bar = [[
                local b = import "baz"
                function bar(): float
                    local p = b.baz()
                    return p.x
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.truthy(ok, err)
    end)

    it("method can call another method of same record", function()
        assert_type_check([[
            record Rec
            end

            function Rec:fn1()
              self:fn2()
            end

            function Rec:fn2()
            end
        ]])
    end)

    it("detects attempts to call non-methods", function()
        assert_type_error("expected record in receiver of method call but found 'integer'", [[
            function fn()
                local i: integer = 0
                i:m()
            end
        ]])
        assert_type_error("method 'm' not found in record 'test.R'", [[
            record R end
            function fn(r: R)
                r:m()
            end
        ]])
        assert_type_error("record type 'test.R' in receiver of method call does not exist", [[
            function fn(r: R)
                r:m()
            end
        ]])
    end)

    it("catches call of external non-method", function ()
        local modules = {
            foo = [[
                a: integer = 1
                record R end
            ]],
            bar = [[
                local f = import "foo"
                function bar(r: f.R, p: f.P)
                    r:m()
                    p:m()
                    f.a:m()
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.falsy(ok)
        assert.match("record type 'foo.P' in receiver of method call does not exist", err)
        assert.match("method 'm' not found in record 'foo.R'", err)
        assert.match("expected record in receiver of method call but found 'integer'", err)
    end)

    it("correctly uses external method", function ()
        local modules = {
            foo = [[
                record R end
                function R:a(): integer
                    return 42
                end
            ]],
            bar = [[
                local f = import "foo"
                function bar(r: f.R): integer
                    return r:a()
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.truthy(ok, err)
    end)

    it("correctly uses transitive external method", function ()
        local modules = {
            baz = [[
                record R end
                function R:a(): integer
                    return 42
                end
            ]],
            foo = [[
                local b = import "baz"
                function f(): b.R
                    return b.R.new()
                end
            ]],
            bar = [[
                local f = import "foo"
                function bar(): integer
                    local r = f.f()
                    return r:a()
                end
            ]]
        }
        local ok, err, mods = run_checker_modules(modules, "bar")
        assert.truthy(ok, err)
    end)

    it("methods cannot have two parameters with the same name", function()
        assert_type_error("duplicate parameter", [[
            record R end
            function R:f(a: integer, a: integer)
            end
        ]])
    end)

    it("typechecks adjustment of argument lists in methods", function()
        assert_type_check([[
            record R end
            function R:g(): (integer, string)
                return 20, "foo"
            end
            function R:f(x: string, y: integer, z: string)
            end
            function R:h()
                self:f("bar", self:g())
            end
        ]])
    end)

    it("catches methods called with wrong arity", function()
        local cases = {{ code = [[
            record R end
            function R:g(): (integer, integer)
                return 20, 30
            end
            function R:f(x: integer, y: integer, z: integer)
            end
            function R:h()
                self:f(self:g())
            end
        ]], args = 2, params = 3 }, { code = [[
            record R end
            function R:g(): (integer, integer)
                return 20, 30
            end
            function R:f(x: integer, y: integer, z: integer)
            end
            function R:h()
                self:f(20, 30, self:g())
            end
        ]], args = 4, params = 3 }, { code = [[
            record R end
            function R:g(): (integer, integer)
                return 20, 30
            end
            function R:f(x: integer, y: integer, z: integer)
            end
            function R:h()
                self:f(20, (self:g()))
            end
        ]], args = 2, params = 3 }}
        for _, c in ipairs(cases) do
            local ok, err = run_checker(c.code)
            assert.falsy(ok)
            assert.match("method 'test.R:f' called with " .. c.args .. " arguments but expects " .. c.params, err)
        end
    end)

    it("typechecks initializer list in record context", function ()
        assert_type_check([[
            record Point
                x: float
                y: float
            end
            function f()
                local p: Point = { x = 1, y = 2 }
            end
        ]])
        assert_type_error("field 'z' not found in record type 'test.Point'", [[
            record Point
                x: float
                y: float
            end
            function f()
                local p: Point = { x = 1, y = 2, z = 3 }
            end
        ]])
        assert_type_error("field 'y' from record type 'test.Point' missing", [[
            record Point
                x: float
                y: float
            end
            function f()
                local p: Point = { x = 1 }
            end
        ]])
        assert_type_error("missing field in initializer for record type 'test.Point'", [[
            record Point
                x: float
                y: float
            end
            function f()
                local p: Point = { x = 1, 2 }
            end
        ]])
        assert_type_error("expected float but found string", [[
            record Point
                x: float
                y: float
            end
            function f()
                local p: Point = { x = 1, y = "foo" }
            end
        ]])
    end)
end)

