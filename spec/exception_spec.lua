local exception = require 'titan-compiler.exception'

describe("Titan exception", function()

    it("can print an exception", function()
        local ex = exception.new("MyException", "error message")
        assert.are.same(tostring(ex), "error message")
    end)

    it("can try", function()
        local ok, err = exception.try(function()
            return "magic"
        end)
        assert.are.same(ok, true)
        assert.are.same(err, "magic")
    end)

    it("can throw an exception", function()
        local ok, err = exception.try(function()
            exception.throw("MyException", "error message")
        end)
        assert.are.same(ok, false)
        assert.are.same(tostring(err), "error message")
    end)

    it("can rethrow an exception", function()
        local ok, err = exception.try(function()
            local ok, err = exception.try(function()
                exception.throw("MyException", "error message")
            end)
            if not ok then
                exception.rethrow(err)
            else
                return err
            end
        end)
        assert.are.same(ok, false)
        assert.are.same(tostring(err), "error message")
    end)
end)
