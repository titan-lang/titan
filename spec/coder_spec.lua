local checker = require 'titan-compiler.checker'
local parser = require 'titan-compiler.parser'
local types = require 'titan-compiler.types'
local coder = require 'titan-compiler.coder'
local util = require 'titan-compiler.util'
local pretty = require 'titan-compiler.pretty'
local driver = require 'titan-compiler.driver'

local verbose = false

local function parse(code)
    return parser.parse("(parser_spec)", code)
end

local function generate_modules(modules, main, forapp)
    types.registry = {}
    local imported = {}
    local loader = driver.tableloader(modules, imported)
    for name, _ in pairs(modules) do
        local type, err = checker.checkimport(name, loader)
        if not type then return nil, err end
        if #err ~= 0 then return nil, table.concat(err, "\n") end
    end
    local mods = {}
    for name, mod in pairs(imported) do
        local ok, err = driver.compile_module(name, mod, nil, forapp, verbose)
        table.insert(mods, name)
        if not ok then return nil, err end
    end
    if forapp then
        local ok, err = driver.compile_program(main, mods, nil, verbose)
        if not ok then return nil, err end
    end
    return true
end

local function call(modname, code)
    local cmd = string.format("lua-5.3.4/src/lua -l %s -e \"print(pcall(function () %s end))\"",
        modname, code)
    local f = io.popen(cmd)
    local out = f:read("*a")
    local ok, err = f:close()
    if not ok then return false, err, out end
    local ok, data = out:match("^(true)%s*(.*)$")
    if not ok then
        local _, error = out:match("^(false)%s*(.*)$")
        return false, error
    else
        return true, data
    end
end

local function run_coder(titan_code, lua_test, errmsg)
    types.registry = {}
    local ast, err = parse(titan_code)
    assert.truthy(ast, err)
    local ok, err = checker.check("test", ast, titan_code, "test.titan")
    assert.equal(0, #err, table.concat(err, "\n"))
    local ok, err = driver.compile("test", ast, nil, nil, nil, verbose)
    assert.truthy(ok, err)
    local ok, err, out = call("test", lua_test)
    if errmsg then
        assert.falsy(ok)
        assert.match(errmsg, err)
    else
        assert.truthy(ok, err)
    end
end

local function call_app(appname, ...)
    local cmd = table.concat({ appname, ... }, " ")
    local f = io.popen(cmd)
    local out = f:read("*a")
    local ok, err, status = f:close()
    if err == "exit" then ok = true end
    if not verbose then os.remove(appname) end
    return ok, err, status, out
end

local function run_coder_app(titan_code, main, estatus, eout)
    types.registry = {}
    local code = string.format([[
        %s
        function main(args: {string}): integer
            %s
        end
    ]], titan_code, main)
    local ast, err = parse(code)
    assert.truthy(ast, err)
    local ok, err = checker.check("test", ast, code, "test.titan")
    assert.equal(0, #err, table.concat(err, "\n"))
    local ok, err = driver.compile("test", ast, nil, nil, true, verbose)
    assert.truthy(ok, err)
    local ok, err = driver.compile_program("test", { "test" }, nil, verbose)
    assert.truthy(ok, err)
    local ok, err, status, output = call_app("./test")
    assert.truthy(ok, err)
    assert.equal(estatus, status)
    if eout then
        assert.match(eout, output)
    end
end

describe("Titan code generator", function()
    setup(function ()
        os.remove("titan-runtime/titan.o")
    end)

    after_each(function ()
        os.execute("rm -f *.o")
        os.execute("rm -f *.so")
        if not verbose then
            os.execute("rm -f *.c")
        end
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
        local ok, err = call("titan_test", [[
            assert(titan_test.geta() == 1)
            assert(titan_test.a == 1)
            titan_test.a = 2
            assert(titan_test.a == 2)
            assert(titan_test.geta() == 2)
        ]])
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
                    if f ~= nil then
                        stdio.fclose(f)
                    end
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

    it("initializes a record and returns a primitive field", function ()
        run_coder([[
            record Point
              x: float
              y: float
            end
            function x(): float
                local r: Point = { x = 2, y = 3 }
                return r.x
            end
            function y(): float
                local r: Point = { x = 2, y = 3 }
                return r.y
            end
        ]], [[
            assert(2.0 == test.x())
            assert(3.0 == test.y())
        ]])
    end)

    it("initializes a record and mutates field", function ()
        run_coder([[
            record Point
              x: float
              y: float
            end
            function x(): float
                local r: Point = { x = 0, y = 0 }
                r.x = 2
                return r.x
            end
            function y(): float
                local r: Point = { x = 0, y = 0 }
                r.y = 3
                return r.y
            end
        ]], [[
            assert(2.0 == test.x())
            assert(3.0 == test.y())
        ]])
    end)

    it("initializes a record and returns a gc field", function ()
        run_coder([[
            record Pair
              x: string
              y: string
            end
            function x(): string
                local r: Pair = { x = 'foo', y = 'bar' }
                return r.x
            end
            function y(): string
                local r: Pair = { x = 'foo', y = 'bar' }
                return r.y
            end
        ]], [[
            assert('foo' == test.x())
            assert('bar' == test.y())
        ]])
    end)

    it("calls method that does not access fields, from outside record", function ()
        run_coder([[
            record Rec
            end
            function Rec:m(): integer
                return 1
            end
            function f(): integer
                local r: Rec = {}
                return r:m()
            end
        ]], [[
            assert(1 == test.f())
        ]])
    end)

    it("calls method that does not access fields, from inside record", function ()
        run_coder([[
            record Rec
            end
            function Rec:m1(): integer
                return self:m2()
            end
            function Rec:m2(): integer
                return 1
            end
            function f(): integer
                local r: Rec = {}
                return r:m1()
            end
        ]], [[
            assert(1 == test.f())
        ]])
    end)

    it("calls record method that accesses fields", function ()
        run_coder([[
            record Point
               x: float
               y: float
            end
            function Point:getx(): float
                return self.x
            end
            function Point:gety(): float
                return self.y
            end
            function f(): (float, float)
                local r: Point = { x = 2, y = 3 }
                return r:getx(), r:gety()
            end
        ]], [[
            local x, y = test.f()
            assert(2.0 == x)
            assert(3.0 == y)
        ]])
    end)

    it("casts from record to value and back successfully", function ()
        run_coder([[
            record Point
              x: float
              y: float
            end
            function f(): float
                local p: Point = { x = 2, y = 0 }
                local v: value = p
                local pp: Point = v
                p.y = 3
                return pp.y
            end
        ]], [[
            assert(3.0 == test.f())
        ]])
    end)

    it("casts from record to value and back unsuccessfully", function ()
        run_coder([[
            record Rec
            end
            record Point
              x: float
              y: float
            end
            function f(): float
                local p: Point = { x = 2, y = 0 }
                local v: value = p
                local r: Rec = v
                return 0
            end
        ]], [[test.f()]], "expected test.Rec but found test.Point")
    end)

    it("casts from value to record unsuccessfully", function ()
        run_coder([[
            record Rec
            end
            function f(): float
                local v: value = 0
                local r: Rec = v
                return 0
            end
        ]], [[test.f()]], "expected test.Rec but found number")
    end)

    it("calls record method that mutates fields", function ()
        run_coder([[
            record Point
               x: float
               y: float
            end
            function Point:getx(): float
                return self.x
            end
            function Point:gety(): float
                return self.y
            end
            function Point:move(dx: float, dy: float)
                self.x, self.y = self.x + dx, self.y + dy
            end
            function f(): (float, float)
                local r: Point = { x = 2, y = 3 }
                r:move(2, 3)
                return r:getx(), r:gety()
            end
        ]], [[
            local x, y = test.f()
            assert(4.0 == x)
            assert(6.0 == y)
        ]])
    end)

    it("uses method of record from another module", function ()
        local modules = {
            foo = [[
                record Point
                  x: float
                  y: float
                end
                function Point:move(dx: float, dy: float)
                    self.x, self.y = self.x + dx, self.y + dy
                end
            ]],
            bar = [[
                local f = import "foo"
                function bar(): (float, float)
                    local p: f.Point = { x = 2, y = 3 }
                    p:move(2, 3)
                    return p.x, p.y
                end
            ]]
        }
        local ok, err = generate_modules(modules, "bar")
        assert.truthy(ok, err)
        local ok, err = call("bar", "x, y = bar.bar(); assert(x == 4.0); assert(y == 6.0)")
        assert.truthy(ok, err)
    end)

    it("uses method of record from transitive module", function ()
        local modules = {
            foo = [[
                record Point
                  x: float
                  y: float
                end
                function Point:move(dx: float, dy: float)
                    self.x, self.y = self.x + dx, self.y + dy
                end
            ]],
            baz = [[
                local f = import "foo"
                function point(x: float, y: float): f.Point
                    return { x = 2, y = 3 }
                end
            ]],
            bar = [[
                local b = import "baz"
                function bar(): (float, float)
                    local p = b.point(2,3)
                    p:move(2, 3)
                    return p.x, p.y
                end
                function foo(): (float, float)
                    local p = b.point(2,3)
                    local v: value = p
                    p = v
                    return p.x, p.y
                end
            ]]
        }
        local ok, err = generate_modules(modules, "bar")
        assert.truthy(ok, err)
        local ok, err = call("bar", "x, y = bar.bar(); assert(x == 4.0); assert(y == 6.0)")
        assert.truthy(ok, err)
        local ok, err = call("bar", "x, y = bar.foo(); assert(x == 2.0); assert(y == 3.0)")
        assert.truthy(ok, err)
    end)

    it("gets record from Titan and sends it back", function ()
        run_coder([[
            record Point
                x: float
                y: float
            end
            function point(): Point
                return { x = 2, y = 3 }
            end
            function unpack(p: Point): (float, float)
                return p.x, p.y
            end
        ]], [[
            local p1 = test.point()
            assert(type(p1) == 'userdata')
            local x, y = test.unpack(p1)
            assert(2.0 == x)
            assert(3.0 == y)
            local p2 = test.point()
            assert(p1 ~= p2)
            assert(getmetatable(p1) == getmetatable(p2))
        ]])
    end)

    it("gets record from titan and sends it back to wrong type", function ()
        run_coder([[
            record Rec
            end
            record Point
              x: float
              y: float
            end
            function point(): Point
                return { x = 2, y = 3 }
            end
            function f(r: Rec)
            end
        ]], [[test.f(test.point())]], "expected test.Rec but found test.Point")
    end)

    it("send wrong type to titan record", function ()
        run_coder([[
            record Rec
            end
            function f(r: Rec)
            end
        ]], [[test.f(2)]], "expected test.Rec but found number")
    end)

    it("gets record from titan inside array", function ()
        run_coder([[
            record Point
                x: float
                y: float
            end
            function points1(): { Point }
                return { { x = 2, y = 3 } }
            end
            function points2(): { Point }
                local ps: { Point } = {}
                ps[1] = { x = 2, y = 3 }
                return ps
            end
            function unpack(p: Point): (float, float)
                return p.x, p.y
            end
        ]], [[
            local ps = test.points1()
            assert(#ps == 1)
            assert(type(ps[1]) == 'userdata')
            local x, y = test.unpack(ps[1])
            assert(2.0 == x)
            assert(3.0 == y)
            local ps = test.points2()
            assert(#ps == 1)
            assert(type(ps[1]) == 'userdata')
            local x, y = test.unpack(ps[1])
            assert(2.0 == x)
            assert(3.0 == y)
        ]])
    end)

    it("gets record from titan through module variable", function ()
        run_coder([[
            record Point
                x: float
                y: float
            end
            p: Point = { x = 2, y = 3 }
            function unpack(p: Point): (float, float)
                return p.x, p.y
            end
        ]], [[
            local p = test.p
            assert(type(p) == 'userdata')
            local x, y = test.unpack(p)
            assert(2.0 == x)
            assert(3.0 == y)
        ]])
    end)

    it("send record to titan through module variable", function ()
        run_coder([[
            record Point
                x: float
                y: float
            end
            p: Point = { x = 0, y = 0 }
            function point(): Point
                return { x = 2, y = 3 }
            end
            function unpack(p: Point): (float, float)
                return p.x, p.y
            end
        ]], [[
            test.p = test.point()
            local x, y = test.unpack(test.p)
            assert(2.0 == x)
            assert(3.0 == y)
        ]])
    end)

    it("access record field from Lua", function ()
        run_coder([[
            record Point
                x: float
                y: float
            end
            record Rec
                x: float
                y: string
                z: Point
            end
            r: Rec = { x = 2, y = 'foo', z = { x = 2, y = 3 } }
            function rec(): Rec
                return { x = 3, y = 'bar', z = { x = 3, y = 2 } }
            end
        ]], [[
            local r1 = test.r
            local r2 = test.rec()
            assert(2.0 == r1.x)
            assert(3.0 == r2.x)
            assert('foo' == r1.y)
            assert('bar' == r2.y)
            assert(2.0 == r1.z.x)
            assert(3.0 == r1.z.y)
            assert(3.0 == r2.z.x)
            assert(2.0 == r2.z.y)
            local ok, err = pcall(function () return r1.foo end)
            assert(not ok)
            assert(err:match('not found'))
        ]])
    end)

    it("mutate record field from Lua", function ()
        run_coder([[
            record Point
                x: float
                y: float
            end
            record Rec
                x: float
                y: string
                z: Point
            end
            r: Rec = { x = 2, y = 'foo', z = { x = 2, y = 3 } }
            function rec(): Rec
                return { x = 3, y = 'bar', z = { x = 3, y = 2 } }
            end
            function getX(r: Rec): float
                return r.x
            end
            function getY(r: Rec): string
                return r.y
            end
            function getZX(r: Rec): float
                return r.z.x
            end
        ]], [[
            local r1 = test.r
            local r2 = test.rec()
            r1.x = 4
            r1.y = 'baz'
            r1.z.x = 5
            r2.x = 8
            r2.y = 'xyz'
            r2.z.x = 10
            assert(4.0 == test.getX(r1))
            assert(8.0 == test.getX(r2))
            assert('baz' == test.getY(r1))
            assert('xyz' == test.getY(r2))
            assert(5.0 == test.getZX(r1))
            assert(10.0 == test.getZX(r2))
            local ok, err = pcall(function () r1.foo = 2 end)
            assert(not ok)
            assert(err:match('not found'))
        ]])
    end)

    it("call record method from Lua", function ()
        run_coder([[
            record Point
                x: float
                y: float
            end
            function Point:move(dx: float, dy: float)
                self.x, self.y = self.x + dx, self.y + dy
            end
            function point(x: float, y: float): Point
                return { x = x, y = y }
            end
        ]], [[
            local p = test.point(2, 3)
            p:move(2, 3)
            assert(4.0 == p.x)
            assert(6.0 == p.y)
            local ok, err = pcall(function () p.move = 2 end)
            assert(not ok)
            assert(err:match('not found'))
        ]])
    end)

    it("initializes a record using new", function ()
        run_coder([[
            record Point
              x: float
              y: float
            end
            function x(): float
                local r: Point = Point.new(2,3)
                return r.x
            end
            function y(): float
                local r: Point = Point.new(2,3)
                return r.y
            end
        ]], [[
            assert(2.0 == test.x())
            assert(3.0 == test.y())
        ]])
    end)

    it("initializes a record using new from Lua", function ()
        run_coder([[
            record Point
              x: float
              y: float
            end
            function unwrap(p: Point): (float, float)
                return p.x, p.y
            end
        ]], [[
            local p = test.Point.new(2, 3)
            local x, y = test.unwrap(p)
            assert(2.0 == x)
            assert(3.0 == y)
        ]])
    end)

    it("constructs record from other module", function ()
        local modules = {
            foo = [[
                record Point
                  x: float
                  y: float
                end
            ]],
            baz = [[
                local f = import "foo"
                function point(x: float, y: float): f.Point
                    return f.Point.new(x, y)
                end
            ]],
            bar = [[
                local b = import "baz"
                function bar(): (float, float)
                    local p = b.point(2,3)
                    p = { x = 4, y = 6 }
                    return p.x, p.y
                end
            ]]
        }
        local ok, err = generate_modules(modules, "bar")
        assert.truthy(ok, err)
        local ok, err = call("bar", "x, y = bar.bar(); assert(x == 4.0); assert(y == 6.0)")
        assert.truthy(ok, err)
    end)

    for _, op in ipairs({"==", "~="}) do
        it("can compare values using " .. op, function()
            run_coder(util.render([[
                function fn(a1: value, a2: value): boolean
                    return a1 $OP a2
                end
            ]], { OP = op }), util.render([[
                local a = {}
                assert(a $OP a == test.fn(a, a))
            ]], { OP = op }))
        end)
    end


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

        it("binary (+) overflow", function()
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

    describe("#maps", function()

        it("adding a value of the wrong type in Lua can trigger a runtime error later in Titan", function ()
            local code = [[
                function f(m: {string:integer}): {string:integer}
                    m["foo"] = m["foo"] + 10
                    return m
                end
            ]]
            local ast, err = parse(code)
            assert.truthy(ast, err)
            local ok, err = checker.check("test", ast, code, "test.titan")
            assert.equal(0, #err, table.concat(err, "\n"))
            local ok, err = driver.compile("titan_test", ast)
            assert.truthy(ok, err)
            local ok, err = call("titan_test", [[
                local m = { foo = 10 }
                local m2 = titan_test.f(m)
                assert(m2 == m)
                assert(m.foo == 20)
                titan_test.f(m)
                assert(m.foo == 30)
                m.foo = 'wat'
                titan_test.f(m) -- supposed to give an error here
            ]])
            assert.falsy(ok)
            assert.match("expected integer but found string", err)
        end)

        local test_types = {
            { t = "value", l1 = "'hello'", l2 = "123" },
            { t = "boolean", v1 = "true", v2 = "false" },
            { t = "integer", v1 = "1984", v2 = "2112" },
            { t = "float", v1 = "2.5", v2 = "3.14" },
            { t = "string", v1 = "'hello'", v2 = "'world'" },
            { t = "{integer}", l1 = "{10, 20}", l2 = "{}" },
            { t = "{string:integer}", l1 = "{x = 10, y = 20}", l2 = "{}"  },
            { t = "Point", l1 = "titan_test.Point.new(10, 20)", l2 = "titan_test.Point.new(30, 40)" },
        }

        for _, t1 in ipairs(test_types) do
            for _, t2 in ipairs(test_types) do
                local map = "{" .. t1.t .. " : " .. t2.t .. "}"

                it("declares a map " .. map, function()
                    local code = util.render([[
                        record Point
                            x: integer
                            y: integer
                        end
                        function new_map(k1: $TK, k2: $TK, v1: $TV, v2: $TV): $MAP
                            local m = { [$K1] = $V1, [$K2] = $V2 }
                            return m
                        end
                    ]], {
                        MAP = map,
                        K1 = t1.v1 or "k1",
                        K2 = t1.v2 or "k2",
                        V1 = t2.v1 or "v1",
                        V2 = t2.v2 or "v2",
                        TK = t1.t,
                        TV = t2.t,
                    })
                    local ast, err = parse(code)
                    assert.truthy(ast, err)
                    local ok, err = checker.check("titan_test", ast, code, "test.titan")
                    assert.equal(0, #err, table.concat(err, "\n"))
                    local ok, err = driver.compile("titan_test", ast)
                    assert.truthy(ok, err)
                    local ok, err = call("titan_test", util.render([[
                        local k1, k2, v1, v2 = $LK1, $LK2, $LV1, $LV2
                        local m = titan_test.new_map(k1, k2, v1, v2)
                        assert(m[$K1] == $V1 and m[$K2] == $V2)
                    ]], {
                        K1 = t1.v1 or "k1",
                        K2 = t1.v2 or "k2",
                        V1 = t2.v1 or "v1",
                        V2 = t2.v2 or "v2",
                        LK1 = t1.l1 or t1.v1,
                        LK2 = t1.l2 or t1.v2,
                        LV1 = t2.l1 or t2.v1,
                        LV2 = t2.l2 or t2.v2,
                    }))
                    assert.truthy(ok, err)
                end)

                it("assigns to a map " .. map, function()
                    local code = util.render([[
                        record Point
                            x: integer
                            y: integer
                        end
                        function new_map(k1: $TK, k2: $TK, v1: $TV, v2: $TV): $MAP
                            local m: $MAP = {}
                            m[$K1] = $V1
                            m[$K2] = $V2
                            return m
                        end
                    ]], {
                        MAP = map,
                        K1 = t1.v1 or "k1",
                        K2 = t1.v2 or "k2",
                        V1 = t2.v1 or "v1",
                        V2 = t2.v2 or "v2",
                        TK = t1.t,
                        TV = t2.t,
                    })
                    local ast, err = parse(code)
                    assert.truthy(ast, err)
                    local ok, err = checker.check("titan_test", ast, code, "test.titan")
                    assert.equal(0, #err, table.concat(err, "\n"))
                    local ok, err = driver.compile("titan_test", ast)
                    assert.truthy(ok, err)
                    local ok, err = call("titan_test", util.render([[
                        local k1, k2, v1, v2 = $LK1, $LK2, $LV1, $LV2
                        local m = titan_test.new_map(k1, k2, v1, v2)
                        assert(m[$K1] == $V1 and m[$K2] == $V2)
                    ]], {
                        K1 = t1.v1 or "k1",
                        K2 = t1.v2 or "k2",
                        V1 = t2.v1 or "v1",
                        V2 = t2.v2 or "v2",
                        LK1 = t1.l1 or t1.v1,
                        LK2 = t1.l2 or t1.v2,
                        LV1 = t2.l1 or t2.v1,
                        LV2 = t2.l2 or t2.v2,
                    }))
                    assert.truthy(ok, err)
                end)

                it("indexes a map " .. map, function()
                    local code = util.render([[
                        record Point
                            x: integer
                            y: integer
                        end
                        function get_map_value(k1: $TK, k2: $TK, v1: $TV, v2: $TV): $TV
                            local m: $MAP = {}
                            m[$K1] = $V1
                            m[$K2] = $V2
                            return m[$K1]
                        end
                    ]], {
                        MAP = map,
                        K1 = t1.v1 or "k1",
                        K2 = t1.v2 or "k2",
                        V1 = t2.v1 or "v1",
                        V2 = t2.v2 or "v2",
                        TK = t1.t,
                        TV = t2.t,
                    })
                    local ast, err = parse(code)
                    assert.truthy(ast, err)
                    local ok, err = checker.check("titan_test", ast, code, "test.titan")
                    assert.equal(0, #err, table.concat(err, "\n"))
                    local ok, err = driver.compile("titan_test", ast)
                    assert.truthy(ok, err)
                    local ok, err = call("titan_test", "v = titan_test.get_map_value(); assert(v == 1984)")
                    local ok, err = call("titan_test", util.render([[
                        local k1, k2, v1, v2 = $LK1, $LK2, $LV1, $LV2
                        local v = titan_test.get_map_value(k1, k2, v1, v2)
                        assert(v == $V1)
                    ]], {
                        K1 = t1.v1 or "k1",
                        K2 = t1.v2 or "k2",
                        V1 = t2.v1 or "v1",
                        V2 = t2.v2 or "v2",
                        LK1 = t1.l1 or t1.v1,
                        LK2 = t1.l2 or t1.v2,
                        LV1 = t2.l1 or t2.v1,
                        LV2 = t2.l2 or t2.v2,
                    }))
                    assert.truthy(ok, err)
                end)
            end
        end

        it("coerces map key", function ()
            run_coder([[
                function fn1(): string
                    local a: { float: string } = {}
                    a[1] = 'foo'
                    return a[1.0]
                end

                function fn2(): string
                    local a: { float: string } = {}
                    a[1.0] = 'foo'
                    return a[1]
                end
            ]], [[
                assert(test.fn1() == 'foo')
                assert(test.fn2() == 'foo')
            ]])
        end)

    end)

    describe("#apps", function()
        it("compiles and runs a titan application", function ()
            run_coder_app([[
                local function fn(m: { string: integer }): integer
                    return m['foo']
                end
            ]], [[
                local m = { ['foo'] = 0 }
                return fn(m)
            ]], 0)
        end)

        it("correctly uses module function in application", function ()
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
                    function main(args: {string}): integer
                        if bar() == 42 then
                            return 0
                        else
                            return 1
                        end
                    end
                ]]
            }
            local ok, err = generate_modules(modules, "bar", true)
            assert.truthy(ok, err)
            local ok, err, status = call_app("./bar")
            assert.truthy(ok, err)
            assert.equal(0, status)
        end)

        it("correctly uses module local variables in application", function ()
            local modules = {
                foo = [[
                    local xs: {integer} = {}
                    function resetxs()
                        xs = {}
                    end
                ]],
                bar = [[
                    local foo = import "foo"
                    function main(args: {string}): integer
                        foo.resetxs()
                        return 42
                    end
                    ]]
                }
            local ok, err = generate_modules(modules, "bar", true)
            assert.truthy(ok, err)
            local ok, err, status = call_app("./bar")
            assert.truthy(ok, err)
            assert.equal(42, status)
        end)

        it("loads titan module from lua code inside application", function ()
            local modules = {
                foo = [[
                    function a(): integer
                        return 42
                    end
                ]],
                bar = [[
                    function main(args: {string}): integer
                        local res = dostring([=[
                            local m = require 'foo'
                            return m.a()
                        ]=])
                        if res[1] == 42 then
                            return 0
                        else
                            return 1
                        end
                    end
                ]]
            }
            local ok, err = generate_modules(modules, "bar", true)
            assert.truthy(ok, err)
            local ok, err, status = call_app("./bar")
            assert.truthy(ok, err)
            assert.equal(0, status)
        end)

    end)

    describe("#builtins", function()
        it("call print", function ()
            run_coder_app([[
            ]], [[
                print(1, 'foo', true, 2.5)
                print()
                return 0
            ]], 0, "1\tfoo\ttrue\t2.5\n\n")
        end)

        it("call assert", function ()
            run_coder([[
                function f(cond: value): value
                    return assert(cond, 'foo')
                end
            ]], [[
                assert(test.f(42) == 42)
                local ok, err = pcall(test.f, nil)
                assert(not ok)
                assert(err:match('foo'))
                local ok, err = pcall(test.f, false)
                assert(not ok)
                assert(err:match('foo'))
            ]])
        end)

        it("call dostring", function ()
            run_coder([=[
                function f(): { value }
                    return dostring([[
                        return ...
                    ]], 1, 'foo', 2.5)
                end
            ]=], [[
                local res = test.f()
                assert(#res == 3)
                assert(res[1] == 1)
                assert(res[2] == 'foo')
                assert(res[3] == 2.5)
            ]])
        end)

        it("call dofile", function ()
            os.execute('echo "return ..." > test_dofile.lua')
            run_coder([=[
                function f(): { value }
                    return dofile('test_dofile.lua', 1, 'foo', 2.5)
                end
            ]=], [[
                local res = test.f()
                assert(#res == 3)
                assert(res[1] == 1)
                assert(res[2] == 'foo')
                assert(res[3] == 2.5)
            ]])
            os.remove("test_dofile.lua")
        end)

        it("call error", function ()
            run_coder([[
                function f(): nil
                    error('foo')
                end
            ]], [[
                local ok, err = pcall(test.f)
                assert(not ok)
                assert(err:match('foo'))
            ]])
        end)

        it("call tostring", function ()
            run_coder([[
                function f(v: value): string
                    return tostring(v)
                end
            ]], [[
                assert(test.f(1) == '1')
                assert(test.f(2.5) == '2.5')
                assert(test.f('foo') == 'foo')
                assert(test.f(true) == 'true')
                assert(test.f(nil) == 'nil')
            ]])
        end)

        it("call tofloat", function ()
            run_coder([[
                function f(v: string): float
                    return tofloat(v)
                end
            ]], [[
                assert(test.f('1') == 1.0)
                assert(test.f('2.5') == 2.5)
                assert(test.f('foo') == 0.0)
            ]])
        end)

        it("call tointeger", function ()
            run_coder([[
                function f(v: string): integer
                    return tointeger(v)
                end
            ]], [[
                assert(test.f('10') == 10)
                assert(test.f('2.5') == 0)
                assert(test.f('foo') == 0)
            ]])
        end)

        it("calls builtin from another module", function ()
            local modules = {
                string = [[
                    builtin function byte(s: string, i: integer): integer
                ]],
                bar = [[
                    import "string"
                    function main(args: {string}): integer
                        if string.byte('foo', 1) == 102 then
                            return 0
                        else
                            return 1
                        end
                    end
                ]]
            }
            local ok, err = generate_modules(modules, "bar", true)
            assert.truthy(ok, err)
            local ok, err, status = call_app("./bar")
            assert.truthy(ok, err)
            assert.equal(0, status)
        end)
    end)

end)


