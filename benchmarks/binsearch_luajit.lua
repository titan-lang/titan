local function binsearch(t, x)
    -- lo <= x <= hi
    local lo = 1
    local hi = #t

    local steps = 0

    while lo < hi do

        local mid = lo + math.floor((hi - lo)/2)
        steps = steps + 1

        if x == t[mid] then
            return steps
        elseif x < t[mid] then
            hi = mid - 1
        else
            lo = mid + 1
        end
    end

    return steps
end

local function test(t)
    local s = 0
    for i = 1, 10000000 do
        if binsearch(t, i) ~= 22 then
            s = s + 1
        end
    end
    return s
end

return {
    binsearch = binsearch,
    test = test,
}
