local checker = require 'titan-compiler.checker'
local parser = require 'titan-compiler.parser'
local types = require 'titan-compiler.types'
local coder = require 'titan-compiler.coder'
local util = require 'titan-compiler.util'
local pretty = require 'titan-compiler.pretty'
local driver = require 'titan-compiler.driver'

local function parse(code)
    return parser.parse("(parser_spec)", code)
end

local function generate_modules(modules, main)
    local imported = {}
    local loader = driver.tableloader(modules, imported)
    local _, errs = checker.checkimport(main, loader)
    if #errs ~= 0 then return nil, table.concat(errs, "\n") end
    for name, mod in pairs(imported) do
        local ok, err = driver.compile_module(name, mod)
        if not ok then return nil, err end
    end
    return true
end

local function call(modname, code)
    local cmd = string.format("lua/src/lua -l %s -e \"%s\"",
        modname, code)
    return os.execute(cmd)
end

local function run_coder(titan_code, lua_test)
    local ast, err = parse(titan_code)
    assert.truthy(ast, err)
    local ok, err = checker.check("test", ast, titan_code, "test.titan")
    assert.equal(0, #err, table.concat(err, "\n"))
    local ok, err = driver.compile("test", ast)
    assert.truthy(ok, err)
    local ok, err = call("test", lua_test)
    assert.truthy(ok, err)
end

describe("Titan code generator", function()
    after_each(function ()
        os.execute("rm -f *.so")
        os.execute("rm -f *.c")
    end)

    it("deletes array element", function()
        local code = [[
            function delete(array: {integer}, i: integer)
                array[i] = nil
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={1,2,3};titan_test.delete(arr,3);assert(#arr==2)")
        assert.truthy(ok, err)
    end)

    it("tests nil element", function()
        local code = [[
            function testset(t: {integer}, i: integer, v: integer): integer
                if t[i] then
                  return t[i]
                else
                  t[i] = v
                  return t[i]
                end
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={};assert(titan_test.testset(arr,1,2)==2);assert(titan_test.testset(arr,1,3)==2)")
        assert.truthy(ok, err)
    end)

    it("tests nil element in 'while'", function()
        local code = [[
            function testfill(t: {integer}, i: integer, v: integer)
                while not t[i] and i > 0 do
                    t[i] = v
                    i = i - 1
                end
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={};titan_test.testfill(arr,5,2);assert(#arr==5)")
        assert.truthy(ok, err)
    end)

    it("tests nil element in 'repeat'", function()
        local code = [[
            function testfill(t: {integer}, i: integer, v: integer)
                repeat
                    t[i] = v
                    i = i - 1
                until t[i] or i == 0
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={};titan_test.testfill(arr,5,2);assert(#arr==5)")
        assert.truthy(ok, err)
    end)

    it("tests integer step value in 'for'", function()
        local code = [[
            function forstep(f: integer, t: integer, s: integer): integer
                local v: integer = 0
                for i = f, t, s do
                    v = v + i
                end
                return v
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "x = titan_test.forstep(1,10,2);assert(x==25)")
        assert.truthy(ok, err)
    end)

    it("tests integer postive literals in 'for'", function()
        local code = [[
            function forstep(): integer
                local v: integer = 0
                for i = 1, 10, 2 do
                    v = v + i
                end
                return v
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "x = titan_test.forstep();assert(x==25)")
        assert.truthy(ok, err)
    end)

    it("tests integer negative literals in 'for'", function()
        local code = [[
            function forstep(): integer
                local v: integer = 0
                for i = 10, 1, -2 do
                    v = v + i
                end
                return v
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "x = titan_test.forstep();assert(x==30)")
        assert.truthy(ok, err)
    end)

    it("tests float step value in 'for'", function()
        local code = [[
            function forstep(f: float, t: float, s: float): float
                local v: float = 0
                for i = f, t, s do
                    v = v + i
                end
                return v
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "x = titan_test.forstep(1.5,10.5,0.5);assert(x==114.0)")
        assert.truthy(ok, err)
    end)

    it("tests float positive literals in 'for'", function()
        local code = [[
            function forstep(): float
                local v: float = 0
                for i = 1.5, 10.5, 0.5 do
                    v = v + i
                end
                return v
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "x = titan_test.forstep();assert(x==114.0)")
        assert.truthy(ok, err)
    end)

    it("tests float negative literals in 'for'", function()
        local code = [[
            function forstep(): float
                local v: float = 0
                for i = 9.5, 1.5, -0.5 do
                    v = v + i
                end
                return v
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "x = titan_test.forstep();assert(x==93.5)")
        assert.truthy(ok, err)
    end)

    it("tests nil element in 'not'", function()
        local code = [[
            function testset(t: {integer}, i: integer, v: integer): integer
                if not t[i] then
                  t[i] = v
                end
                return t[i]
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={};assert(titan_test.testset(arr,1,2)==2);assert(titan_test.testset(arr,1,3)==2)")
        assert.truthy(ok, err)
    end)

    it("tests nil element in 'and'", function()
        local code = [[
            function testset(t: {integer}, i: integer, v: integer): integer
                if t[i] and v then
                  return t[i]
                else
                  t[i] = v
                  return t[i]
                end
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={};assert(titan_test.testset(arr,1,2)==2);assert(titan_test.testset(arr,1,3)==2)")
        assert.truthy(ok, err)
    end)

    it("tests nil element in 'or'", function()
        local code = [[
            function testset(t: {integer}, i: integer, v: integer): integer
                if not t[i] or not t[i] then
                  t[i] = v
                end
                return t[i]
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={};assert(titan_test.testset(arr,1,2)==2);assert(titan_test.testset(arr,1,3)==2)")
        assert.truthy(ok, err)
    end)

    it("tests 'or' pattern", function()
        local code = [[
            function getor(t: {integer}, i: integer, v: integer): integer
                return t[i] or v
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={};assert(titan_test.getor(arr,1,2)==2);arr[1]=2;assert(titan_test.getor(arr,1,3)==2)")
        assert.truthy(ok, err)
    end)

    it("tests 'and' pattern", function()
        local code = [[
            function ternary(t: {integer}, i: integer, v1: integer, v2: integer): integer
                return t[i] and v1 or v2
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={};assert(titan_test.ternary(arr,1,3,2)==2);arr[1]=2;assert(titan_test.ternary(arr,1,2,3)==2)")
        assert.truthy(ok, err)
    end)

    it("pass integers when expecting floats in array", function()
        local code = [[
            function sum(array: {float}): float
                local res = 0.0
                for i = 1, #array do
                    res = res + array[i]
                end
                return res
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "arr={1,2,3};assert(6==titan_test.sum(arr))")
        assert.truthy(ok, err)
    end)

    it("pass integers when expecting floats in argument", function()
        local code = [[
            function sum(a: float, b: float, c: float): float
                return a + b + c
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(6==titan_test.sum(1,2,3))")
        assert.truthy(ok, err)
    end)

    it("generates code for exponentiation", function()
        local code = [[
            function power(a: float, b: float): float
                return a ^ b
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.power(2,3) == 8)")
        assert.truthy(ok, err)
    end)

    it("generates code for returning 'if'", function()
        local code = [[
			function abs(x:integer): integer
    			if x < 0 then return -x end
    			return x
			end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("titan_test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.abs(-1) == 1);assert(titan_test.abs(0) == 0);assert(titan_test.abs(1) == 1)")
        assert.truthy(ok, err)
    end)

    it("generates code for 'elseif'", function()
        local code = [[
            function getval(a: integer): integer
                if a == 1 then
                    return 10
                elseif a == 2 then
                    return 20
                else
                    return 30
                end
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.getval(1) == 10);assert(titan_test.getval(2) == 20);assert(titan_test.getval(3) == 30)")
        assert.truthy(ok, err)
    end)

    it("generates code for 'elseif' with overlapping conditions", function()
        local code = [[
            function getval(a: integer): integer
                local b = 0
                if a > 2 then
                    b = 10
                elseif a > 1 then
                    b = 20
                else
                    b = 30
                end
                return b
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.getval(2) == 20);assert(titan_test.getval(3) == 10);assert(titan_test.getval(1) == 30)")
        assert.truthy(ok, err)
    end)

    it("generates code for integer module-local variables", function()
        local code = [[
            local a: integer = 1
            function geta(): integer
                return a
            end
            function seta(x: integer)
                a = x
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.geta() == 1);titan_test.seta(2);assert(titan_test.geta() == 2)")
        assert.truthy(ok, err)
    end)

    it("generates code for float module-local variables", function()
        local code = [[
            local a: float = 1
            function geta(): float
                return a
            end
            function seta(x: float)
                a = x
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.geta() == 1);titan_test.seta(2);assert(titan_test.geta() == 2)")
        assert.truthy(ok, err)
    end)

    it("generates code for boolean module-local variables", function()
        local code = [[
            local a: boolean = true
            function geta(): boolean
                return a
            end
            function seta(x: boolean)
                a = x
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.geta() == true);titan_test.seta(false);assert(titan_test.geta() == false)")
        assert.truthy(ok, err)
    end)

    it("generates code for array module-local variables", function()
        local code = [[
            local a: {integer} = {}
            function len(): integer
                return #a
            end
            function seta(x: {integer})
                a = x
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.len() == 0);titan_test.seta({1});assert(titan_test.len() == 1)")
        assert.truthy(ok, err)
    end)

    it("handles coercion to integer", function()
        local code = [[
            function fn(): integer
                local f: float = 1.0
                local i: integer = f
                return i
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "local x = titan_test.fn(); assert(math.type(x) == 'integer')")
        assert.truthy(ok, err)
    end)

    it("handles unused locals", function()
        local code = [[
            function fn()
                local f: float = 1.0
                local i: integer = f
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
    end)

    it("generates code for integer exported variables", function()
        local code = [[
            a: integer = 1
            function geta(): integer
                return a
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.geta() == 1);titan_test.a = 2;assert(titan_test.geta() == 2)")
        assert.truthy(ok, err)
    end)

    it("generates code for exported float variables", function()
        local code = [[
            a: float = 1
            function geta(): float
                return a
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.geta() == 1);titan_test.a = 2;assert(titan_test.geta() == 2)")
        assert.truthy(ok, err)
    end)

    it("generates code for exported boolean variables", function()
        local code = [[
            a: boolean = true
            function geta(): boolean
                return a
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.geta() == true);titan_test.a = false;assert(titan_test.geta() == false)")
        assert.truthy(ok, err)
    end)

    it("generates code for exported array variables", function()
        local code = [[
            a: {integer} = {}
            function len(): integer
                return #a
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.len() == 0);titan_test.a={1};assert(titan_test.len() == 1)")
        assert.truthy(ok, err)
    end)

    it("generates code for string length", function()
        local code = [[
            function len(a: string): integer
                return #a
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.len('foobar') == 6)")
        assert.truthy(ok, err)
    end)

    it("generates code for string literals", function()
        local code = [[
            function fn(): string
                --return "foo\tbar\nbaz"
                local x: string = "foo"
                return x
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        --local ok, err = call("titan_test", "assert(titan_test.lit() == 'foo\\tbar\\nbaz')")
        local ok, err = call("titan_test", "local x = titan_test.fn(); assert(x == 'foo')")
        assert.truthy(ok, err)
    end)

    it("generates code for string concatenation", function()
        local code = [[
            function concat(a: string): string
                return a .. "foo"
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.concat('a') == 'afoo')")
        assert.truthy(ok, err)
    end)

    it("generates code for string coercion from integer", function()
        local code = [[
            function concat(a: string): string
                return a .. 2
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.concat('a') == 'a2')")
        assert.truthy(ok, err)
    end)

    it("generates code for string coercion from float", function()
        local code = [[
            function concat(a: string): string
                return a .. 2.5
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.concat('a') == 'a2.5')")
        assert.truthy(ok, err)
    end)

    it("generates code for string concatenation of several strings", function()
        local code = [[
            function concat(a: string, b: string, c: string, d: string, e: string): string
                return a .. b .. c .. d .. e
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.concat('a','b','c','d','e') == 'abcde')")
        assert.truthy(ok, err)
    end)

    it("generates code for string concatenation resulting in long string", function()
        local code = [[
            function concat(a: string, b: string, c: string, d: string, e: string): string
                return a .. b .. c .. d .. e
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", "assert(titan_test.concat('aaaaaaaaaa','bbbbbbbbbb','cccccccccc','dddddddddd','eeeeeeeeee') == 'aaaaaaaaaabbbbbbbbbbccccccccccddddddddddeeeeeeeeee')")
        assert.truthy(ok, err)
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
        local ok, err = generate_modules(modules, "bar")
        assert.truthy(ok, err)
        local ok, err = call("bar", "assert(bar.bar() == 42)")
        assert.truthy(ok, err)
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
        local ok, err = generate_modules(modules, "bar")
        assert.truthy(ok, err)
        local ok, err = call("bar", "assert(bar.bar() == 5); assert((require 'foo').a == 5)")
        assert.truthy(ok, err)
    end)

    local tovalue = {
        integer = 1,
        float = 1.5 ,
        boolean = true,
        string = "'foo'",
        ["nil"] = "nil",
        array = { type = "{integer}", val = "{1,2,3}", test = "x[3] == 3" }
    }

    for tag, val in pairs(tovalue) do
        it("handles coercion to value from " .. tag, function ()
            local typ = type(val) == "table" and val.type or tag
            local v = type(val) == "table" and val.val or tostring(val)
            local test = type(val) == "table" and val.test or "x == " .. tostring(val)
            local code = util.render([[
                function fn(): value
                    local x: $TYPE = $VAL
                    return x
                end
            ]], { TYPE = typ, VAL = v })
            local ast, err = parse(code)
            assert.truthy(ast, err)
            local ok, err = checker.check("test", ast, code, "test.titan")
            assert.truthy(ok)
            assert.are.same(0, #err)
            local ok, err = driver.compile("titan_test", ast)
            assert.truthy(ok, err)
            local code = 'local x = titan_test.fn(); assert(' .. test .. ')'
            local ok, err = call("titan_test", code)
            assert.truthy(ok, err)
        end)

        it("handles coercion from value to " .. tag, function ()
            local typ = type(val) == "table" and val.type or tag
            local v = type(val) == "table" and val.val or tostring(val)
            local test = type(val) == "table" and val.test or "x == " .. tostring(val)
            local code = util.render([[
                function fn(): $TYPE
                    local x: value = $VAL
                    return x
                end
            ]], { TYPE = typ, VAL = v })
            local ast, err = parse(code)
            assert.truthy(ast, err)
            local ok, err = checker.check("test", ast, code, "test.titan")
            assert.truthy(ok)
            assert.are.same(#err, 0)
            local ok, err = driver.compile("titan_test", ast)
            assert.truthy(ok, err)
            local code = 'local x = titan_test.fn(); assert(' .. test .. ')'
            local ok, err = call("titan_test", code)
            assert.truthy(ok, err)
        end)

        it("handles coercion from value element to " .. tag, function ()
            local typ = type(val) == "table" and val.type or tag
            local v = type(val) == "table" and val.val or tostring(val)
            local test = type(val) == "table" and val.test or "x == " .. tostring(val)
            local code = util.render([[
                function fn(): $TYPE
                    local x: { value } = { $VAL }
                    return x[1]
                end
            ]], { TYPE = typ, VAL = v })
            local ast, err = parse(code)
            assert.truthy(ast, err)
            local ok, err = checker.check("test", ast, code, "test.titan")
            assert.truthy(ok)
            assert.are.same(#err, 0)
            local ok, err = driver.compile("titan_test", ast)
            assert.truthy(ok, err)
            local code = 'local x = titan_test.fn(); assert(' .. test .. ')'
            local ok, err = call("titan_test", code)
            assert.truthy(ok, err)
        end)
    end

    it("handles coercion between arrays of values and other arrays", function ()
        local code = util.render([[
            function fn(): { value }
                local x: { integer } = { 1, 2, 3 }
                local y: { value } = x
                local z: { integer } = y
                return z
            end
        ]], { TYPE = typ, VAL = v })
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.truthy(ok)
        assert.are.same(#err, 0)
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local code = 'local x = titan_test.fn(); assert(x[3] == 3)'
        local ok, err = call("titan_test", code)
        assert.truthy(ok, err)
    end)

    local valfailures = {
        integer = "'foo'",
        float = "'foo'",
        string = 2,
        ["nil"] = 0,
        table = { type = "{integer}", val = "10" }
    }

    for tag, val in pairs(valfailures) do
        it("handles coercion failure from value to " .. tag, function ()
            local typ = type(val) == "table" and val.type or tag
            local v = type(val) == "table" and val.val or tostring(val)
            local code = util.render([[
                function fn(): $TYPE
                    local x: value = $VAL
                    return x
                end
            ]], { TYPE = typ, VAL = v })
            local ast, err = parse(code)
            assert.truthy(ast, err)
            local ok, err = checker.check("test", ast, code, "test.titan")
            assert.truthy(ok)
            assert.are.same(#err, 0)
            local ok, err = driver.compile("titan_test", ast)
            assert.truthy(ok, err)
            local code = "local ok, err = pcall(titan_test.fn); assert(not ok); assert(err:match('expected " .. tag .. "'))"
            local ok, err = call("titan_test", code)
            assert.truthy(ok, err)
        end)

        it("handles coercion failure from value element to " .. tag, function ()
            local typ = type(val) == "table" and val.type or tag
            local v = type(val) == "table" and val.val or tostring(val)
            local code = util.render([[
                function fn(): $TYPE
                    local x: {value} = {$VAL}
                    return x[1]
                end
            ]], { TYPE = typ, VAL = v })
            local ast, err = parse(code)
            assert.truthy(ast, err)
            local ok, err = checker.check("test", ast, code, "test.titan")
            assert.truthy(ok)
            assert.are.same(#err, 0)
            local ok, err = driver.compile("titan_test", ast)
            assert.truthy(ok, err)
            local code = "local ok, err = pcall(titan_test.fn); assert(not ok); assert(err:match('expected " .. tag .. "'))"
            local ok, err = call("titan_test", code)
            assert.truthy(ok, err)
        end)
    end

    it("pass value type in array index", function()
        local code = [[
            function read(array: {float}, i: value): float
                return array[i]
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", [[
            arr={1,2,3}
            assert(2==titan_test.read(arr, 2))
            assert(2==titan_test.read(arr, 2.0))
            assert(pcall(titan_test.read, arr, "foo") == false)
        ]])
        assert.truthy(ok, err)
    end)

    it("casts from integer to boolean", function()
        local code = [[
            function f (a:integer, b:integer): boolean
                return a and b
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", [[
            assert(true==titan_test.f(2, 3))
        ]])
        assert.truthy(ok, err)
    end)

    it("loads C headers with foreign import", function ()
        local modules = {
            foo = [[
                local stdio = foreign import "stdio.h"
                function fun()
                    local fd = stdio.fopen("tmp_test_hello.txt", "w")
                    local msg = "Hello, Titan!\n"
                    local n = stdio.fwrite(msg, #msg, 1, fd)
                    local err = stdio.fclose(fd)
                end
            ]]
        }
        local ok, err = generate_modules(modules, "foo")
        assert.truthy(ok, err)
        local ok, err = call("foo", [[
            os.remove('tmp_test_hello.txt')
            foo.fun()
            local fd = io.open('tmp_test_hello.txt', 'r')
            local data = fd:read('*a')
            fd:close()
            os.remove('tmp_test_hello.txt')
            assert(data == 'Hello, Titan!\n')
        ]])
        assert.truthy(ok, err)
    end)

    it("can cast a string to void pointer in foreign calls", function ()
        local modules = {
            foo = [[
                local stdio = foreign import "stdio.h"
                local stdlib = foreign import "stdlib.h"
                local string_h = foreign import "string.h"
                function fun()
                    local mem = stdlib.realloc(nil, 100)
                    local h = "Hello, cast"
                    local s = string_h.strcpy(mem as string, h)
                    stdlib.free(mem)
                    local fd = stdio.fopen("tmp_test_hello.txt", "w")
                    local n = stdio.fwrite(s, 11, 1, fd)
                    local err = stdio.fclose(fd)
                end
            ]]
        }
        local ok, err = generate_modules(modules, "foo")
        assert.truthy(ok, err)
        local ok, err = call("foo", [[
            os.remove('tmp_test_hello.txt')
            foo.fun()
            local fd = io.open('tmp_test_hello.txt', 'r')
            local data = fd:read('*a')
            fd:close()
            os.remove('tmp_test_hello.txt')
            assert(data == 'Hello, cast')
        ]])
        assert.truthy(ok, err)
    end)

    it("can read foreign global variables", function ()
        local modules = {
            foo = [[
                local stdio = foreign import "stdio.h"
                local errno = foreign import "errno.h"
                function fun(name: string): integer
                    local f = stdio.fopen(name, "r")
                    return errno.errno
                end
            ]]
        }
        local ok, err = generate_modules(modules, "foo")
        assert.truthy(ok, err)
        local ok, err = call("foo", [[
            os.remove('file_that_doesnt_exist.txt')
            assert(foo.fun('file_that_doesnt_exist.txt') > 0)
        ]])
        assert.truthy(ok, err)
    end)

    it("can use foreign defines", function ()
        local modules = {
            foo = [[
                local stdio = foreign import "stdio.h"
                local errno = foreign import "errno.h"
                function enoent(): integer
                    return errno.ENOENT
                end
                function fun(name: string, err: integer): boolean
                    local f = stdio.fopen(name, "r")
                    if f ~= nil then
                        stdio.fclose(f)
                    end
                    return errno.errno == err
                end
            ]]
        }
        local ok, err = generate_modules(modules, "foo")
        assert.truthy(ok, err)
        local ok, err = call("foo", [[
            os.remove('file_that_doesnt_exist.txt')
            assert(foo.fun('file_that_doesnt_exist.txt', foo.enoent()))
        ]])
        assert.truthy(ok, err)
    end)

    it("and between two integers", function()
        local code = [[
            function f (a:integer, b:integer): integer
                return a and b
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", [[
            assert(3==titan_test.f(2, 3))
        ]])
        assert.truthy(ok, err)
    end)

    it("or between two integers", function()
        local code = [[
            function f (a:integer, b:integer): integer
                return a or b
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", [[
            assert(2==titan_test.f(2, 3))
        ]])
        assert.truthy(ok, err)
    end)

    it("calls function with multiple returns from Lua", function()
        local code = [[
            function f(x: integer): (integer, string)
                return x * 2, "foo"
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", [[
            local x, y = titan_test.f(2)
            assert(4 == x)
            assert('foo' == y)
            assert(pcall(titan_test.f, 2, 'foo') == false)
        ]])
        assert.truthy(ok, err)
    end)

    it("calls function with multiple returns from Titan declaration", function()
        local code = [[
            function f(x: integer): (integer, string)
                return x * 2, "foo"
            end

            function g1(): integer
                local x, _ = f(2)
                return x
            end

            function g2(): integer
                local x = (f(2))
                return x
            end

            function g3(): integer
                return (f(2))
            end

            function g4(): integer
                return (f(2))
            end

            function h(): string
                local x, y = f(2)
                return y
            end

            function cf1(x: integer, y: string): integer
                return x
            end

            function cf2(x: integer, y: string): string
                return y
            end

            function i(): integer
                return cf1(f(2))
            end

            function j(): string
                return cf2(f(2))
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", [[
            assert(4 == titan_test.g1())
            assert(4 == titan_test.g2())
            assert(4 == titan_test.g3())
            assert(4 == titan_test.g4())
            assert('foo' == titan_test.h())
            assert(4 == titan_test.i())
            assert('foo' == titan_test.j())
        ]])
        assert.truthy(ok, err)
    end)

    it("handles multiple return values in array initialization", function ()
        local code = [[
            function f(): (integer, integer)
                return 40, 50
            end
            function g(): (integer, string)
                return 20, "foo"
            end
            function fn(): {integer}
                local arr: {integer} = { 10, g(), 30, f() }
                return arr
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err, table.concat(err, "\n"))
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", [[
            local arr = titan_test.fn()
            assert(5 == #arr)
            assert(10 == arr[1])
            assert(20 == arr[2])
            assert(30 == arr[3])
            assert(40 == arr[4])
            assert(50 == arr[5])
        ]])
        assert.truthy(ok, err)
    end)

    it("handles multiple assignment", function()
        local code = [[
            function f(x: integer): (integer, string)
                return x * 2, "foo"
            end

            function m1(): integer
                local x, y = 0, ""
                x, y = f(2)
                return x
            end

            function m2(): string
                local x, y = 0, ""
                x, y = f(2)
                return y
            end

            function m3(): integer
                local x, y = 0, ""
                x, y = 4, "foo"
                return x
            end

            function m4(): string
                local x, y = 0, ""
                x, y = 4, "foo"
                return y
            end

            function m5(): integer
                local x: {integer} = {}
                x[1], x[2] = 4, 2
                return x[1]
            end

            function m6(): integer
                local x: {integer} = {}
                x[1], x[2] = 2, 4
                return x[2]
            end

            function m7(): string
                local x: {string} = {}
                x[1], x[2] = "foo", ""
                return x[1]
            end

            function m8(): string
                local x: {string} = {}
                x[1], x[2] = "", "foo"
                return x[2]
            end

            function m9(): integer
                local x, y = 2, 4
                x, y = y, x
                return x
            end

            function m10(): string
                local x, y = "bar", "foo"
                x, y = y, x
                return x
            end
        ]]
        local ast, err = parse(code)
        assert.truthy(ast, err)
        local ok, err = checker.check("test", ast, code, "test.titan")
        assert.equal(0, #err)
        local ok, err = driver.compile("titan_test", ast)
        assert.truthy(ok, err)
        local ok, err = call("titan_test", [[
            assert(4 == titan_test.m1())
            assert(4 == titan_test.m3())
            assert(4 == titan_test.m5())
            assert(4 == titan_test.m6())
            assert(4 == titan_test.m9())
            assert('foo' == titan_test.m2())
            assert('foo' == titan_test.m4())
            assert('foo' == titan_test.m7())
            assert('foo' == titan_test.m8())
            assert('foo' == titan_test.m10())
        ]])
        assert.truthy(ok, err)
    end)

    it("correctly uses module variable in multiple assignment", function ()
        local modules = {
            foo = [[
                a: integer = 1
            ]],
            bar = [[
                local foo = import "foo"
                function bar(): integer
                    local x = 0
                    x, foo.a = 2, 5
                    return foo.a
                end
            ]]
        }
        local ok, err = generate_modules(modules, "bar")
        assert.truthy(ok, err)
        local ok, err = call("bar", "assert(bar.bar() == 5); assert((require 'foo').a == 5)")
        assert.truthy(ok, err)
    end)

    describe("Lua vs C operator semantics", function()
        it("unary (-)", function()
            run_coder([[
                function f(x:integer): integer
                    return -x
                end
            ]], [[
                assert(math.mininteger == test.f(math.mininteger))
            ]])
        end)

        it("unary (+)", function()
            run_coder([[
                function f(x:integer, y:integer): integer
                    return x + y
                end
            ]], [[
                assert(math.mininteger == test.f(math.maxinteger, 1))
                assert(math.maxinteger == test.f(math.mininteger, -1))
            ]])
        end)

        it("(//)", function()
            run_coder([[
                function f(x:integer, y:integer): integer
                    return x // y
                end
            ]], [[
                assert( 10 //  3 == test.f( 10,  3))
                assert( 10 // -3 == test.f( 10, -3))
                assert(-10 //  3 == test.f(-10,  3))
                assert(-10 // -3 == test.f(-10, -3))
                assert(math.mininteger == test.f(math.mininteger, -1))
            ]])
        end)

        it("(%)", function()
            run_coder([[
                function f(x:integer, y:integer): integer
                    return x % y
                end
            ]], [[
                assert( 10 %  3 == test.f( 10,  3))
                assert( 10 % -3 == test.f( 10, -3))
                assert(-10 %  3 == test.f(-10,  3))
                assert(-10 % -3 == test.f(-10, -3))
                assert(0 == test.f(math.mininteger, -1))
            ]])
        end)

        it("(>>)", function()
            run_coder([[
                function f(x:integer, y:integer): integer
                    return x >> y
                end
            ]], [[
                assert(0xdead >>  1 == test.f(0xdead,  1))
                assert(0xdead >> -1 == test.f(0xdead, -1))
                assert(0 == test.f(0xdead,  100))
                assert(0 == test.f(0xdead, -100))
                assert(0 == test.f(0xdead, math.maxinteger))
                assert(0 == test.f(0xdead, math.mininteger))
            ]])
        end)
end)

end)


