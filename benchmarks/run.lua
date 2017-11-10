
if not os.execute("which luajit > /dev/null") then
	print("Please install LuaJIT for running the benchmarks.")
	os.exit(1)
end

local path = "benchmarks/"

for filename in io.popen("ls -1 *.titan"):lines() do
	local b = filename:match("([^/]*)[.]titan$")
	local benchmark = " driver_" .. b .. ".lua "
	print("Running " .. b)
	print("Titan")
	local titan_path = path .. b
	local cmd = "cd .. && ./titanc %s.titan >/dev/null 2>&1 && cd %s"
	cmd = string.format(cmd, titan_path, path)
	os.execute(cmd)
	os.execute("lua" .. benchmark .. "titan")
	os.execute("rm -f " .. b .. ".c " .. b .. ".so")
	print("LuaJIT")
	os.execute("luajit" .. benchmark)
	print("Lua")
	os.execute("lua" .. benchmark)
end

os.exit(0)
