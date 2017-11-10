local bs

if arg[1] == "titan" then
    bs = require "binsearch"
else
    bs = require "binsearch_luajit"
end

local t = {}
for x = 1, 1000000 do
    t[x] = x
end

local t1 = os.clock()
bs.test(t)
local t2 = os.clock()

print(t2 - t1)
