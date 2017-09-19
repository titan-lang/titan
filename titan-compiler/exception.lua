--
-- Exception handling

local exception = {}

local exceptionmt = {
    __tostring = function(exception)
        if exception.traceback then
            return exception.msg .. exception.traceback
        else
            return exception.msg
        end
    end
}

function exception.new(tag, msg, traceback)
    return setmetatable({tag=tag, msg=msg, traceback=traceback}, exceptionmt)
end

function exception.throw(tag, msg)
    error(exception.new(tag, msg), 2)
end

function exception.rethrow(exc)
    error(exc)
end

local function errhandler(err)
    if getmetatable(err) ~= exceptionmt then
        return exception.new('Error', err, debug.traceback('', 3))
    else
        return err
    end
end

-- Execute a function with protection (like pcall), but if an error occurs
-- always return an exception
function exception.try(block)
    return xpcall(block, errhandler)
end

return exception
