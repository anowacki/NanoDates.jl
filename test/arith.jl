@testset "add periods" begin
    @test ananodate + Year(1) == NanoDate(yr + 1, mn, dy, hr, mi, sc, ms, cs, ns)
    @test ananodate + Month(1) == NanoDate(yr, mn + 1, dy, hr, mi, sc, ms, cs, ns)
    @test ananodate + Day(1) == NanoDate(yr, mn, dy + 1, hr, mi, sc, ms, cs, ns)
    @test ananodate + Hour(1) == NanoDate(yr, mn, dy, hr + 1, mi, sc, ms, cs, ns)
    @test ananodate + Minute(1) == NanoDate(yr, mn, dy, hr, mi + 1, sc, ms, cs, ns)
    @test ananodate + Second(1) == NanoDate(yr, mn, dy, hr, mi, sc + 1, ms, cs, ns)
    @test ananodate + Millisecond(1) == NanoDate(yr, mn, dy, hr, mi, sc, ms + 1, cs, ns)
    @test ananodate + Microsecond(1) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs + 1, ns)
    @test ananodate + Nanosecond(1) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs, ns + 1)
end

@testset "subtract periods" begin
    @test ananodate - Year(1) == NanoDate(yr - 1, mn, dy, hr, mi, sc, ms, cs, ns)
    @test ananodate - Month(1) == NanoDate(yr, mn - 1, dy, hr, mi, sc, ms, cs, ns)
    @test ananodate - Day(1) == NanoDate(yr, mn, dy - 1, hr, mi, sc, ms, cs, ns)
    @test ananodate - Hour(1) == NanoDate(yr, mn, dy, hr - 1, mi, sc, ms, cs, ns)
    @test ananodate - Minute(1) == NanoDate(yr, mn, dy, hr, mi - 1, sc, ms, cs, ns)
    @test ananodate - Second(1) == NanoDate(yr, mn, dy, hr, mi, sc - 1, ms, cs, ns)
    @test ananodate - Millisecond(1) == NanoDate(yr, mn, dy, hr, mi, sc, ms - 1, cs, ns)
    @test ananodate - Microsecond(1) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs - 1, ns)
    @test ananodate - Nanosecond(1) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs, ns - 1)
end

@testset "trunc to period" begin
    @test trunc(ananodate, Year) == NanoDate(yr)
    @test trunc(ananodate, Month) == NanoDate(yr, mn)
    @test trunc(ananodate, Day) == NanoDate(yr, mn, dy)
    @test trunc(ananodate, Hour) == NanoDate(yr, mn, dy, hr)
    @test trunc(ananodate, Minute) == NanoDate(yr, mn, dy, hr, mi)
    @test trunc(ananodate, Second) == NanoDate(yr, mn, dy, hr, mi, sc)
    @test trunc(ananodate, Millisecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms)
    @test trunc(ananodate, Microsecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs)
    @test trunc(ananodate, Nanosecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs, ns)
end

@testset "floor to period" begin
    @test floor(ananodate, Year) == NanoDate(yr)
    @test floor(ananodate, Month) == NanoDate(yr, mn)
    @test floor(ananodate, Day) == NanoDate(yr, mn, dy)
    @test floor(ananodate, Hour) == NanoDate(yr, mn, dy, hr)
    @test floor(ananodate, Minute) == NanoDate(yr, mn, dy, hr, mi)
    @test floor(ananodate, Second) == NanoDate(yr, mn, dy, hr, mi, sc)
    @test floor(ananodate, Millisecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms)
    @test floor(ananodate, Microsecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs)
    @test floor(ananodate, Nanosecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs, ns)
end

@testset "ceil to period" begin
    @test ceil(ananodate, Year) == NanoDate(yr + !isone(mn))
    @test ceil(ananodate, Month) == NanoDate(yr, mn + !isone(dy))
    @test ceil(ananodate, Day) == NanoDate(yr, mn, dy + !iszero(hr))
    @test ceil(ananodate, Hour) == NanoDate(yr, mn, dy, hr + !iszero(mi))
    @test ceil(ananodate, Minute) == NanoDate(yr, mn, dy, hr, mi + !iszero(sc))
    @test ceil(ananodate, Second) == NanoDate(yr, mn, dy, hr, mi, sc + !iszero(ms))
    @test ceil(ananodate, Millisecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms + !iszero(cs))
    @test ceil(ananodate, Microsecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs + !iszero(ns))
    @test ceil(ananodate, Nanosecond) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs, ns)
end

@testset "round down to period" begin
    @test round(ananodate, Year, RoundDown) == NanoDate(yr)
    @test round(ananodate, Month, RoundDown) == NanoDate(yr, mn)
    @test round(ananodate, Day, RoundDown) == NanoDate(yr, mn, dy)
    @test round(ananodate, Hour, RoundDown) == NanoDate(yr, mn, dy, hr)
    @test round(ananodate, Minute, RoundDown) == NanoDate(yr, mn, dy, hr, mi)
    @test round(ananodate, Second, RoundDown) == NanoDate(yr, mn, dy, hr, mi, sc)
    @test round(ananodate, Millisecond, RoundDown) == NanoDate(yr, mn, dy, hr, mi, sc, ms)
    @test round(ananodate, Microsecond, RoundDown) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs)
    @test round(ananodate, Nanosecond, RoundDown) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs, ns)
end

@testset "round up to period" begin
    @test round(ananodate, Year, RoundUp) == NanoDate(yr + !isone(mn))
    @test round(ananodate, Month, RoundUp) == NanoDate(yr, mn + !isone(dy))
    @test round(ananodate, Day, RoundUp) == NanoDate(yr, mn, dy + !iszero(hr))
    @test round(ananodate, Hour, RoundUp) == NanoDate(yr, mn, dy, hr + !iszero(mi))
    @test round(ananodate, Minute, RoundUp) == NanoDate(yr, mn, dy, hr, mi + !iszero(sc))
    @test round(ananodate, Second, RoundUp) == NanoDate(yr, mn, dy, hr, mi, sc + !iszero(ms))
    @test round(ananodate, Millisecond, RoundUp) == NanoDate(yr, mn, dy, hr, mi, sc, ms + !iszero(cs))
    @test round(ananodate, Microsecond, RoundUp) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs + !iszero(ns))
    @test round(ananodate, Nanosecond, RoundUp) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs, ns)
end

@testset "round nearest (ties up) to period" begin
    @test round(ananodate, Year, RoundUp, RoundNearestTiesUp) == NanoDate(yr + (mn >= 6))
    @test round(ananodate, Month, RoundUp, RoundNearestTiesUp) == NanoDate(yr, mn + (dy >= daysinmonth(yr, mn)/2))
    @test round(ananodate, Day, RoundUp, RoundNearestTiesUp) == NanoDate(yr, mn, dy + (hr >= 12))
    @test round(ananodate, Hour, RoundUp, RoundNearestTiesUp) == NanoDate(yr, mn, dy, hr + (mn >= 30))
    @test round(ananodate, Minute, RoundUp, RoundNearestTiesUp) == NanoDate(yr, mn, dy, hr, mi + (sc >= 30))
    @test round(ananodate, Second, RoundUp, RoundNearestTiesUp) == NanoDate(yr, mn, dy, hr, mi, sc + (ms >= 500))
    @test round(ananodate, Millisecond, RoundUp, RoundNearestTiesUp) == NanoDate(yr, mn, dy, hr, mi, sc, ms + (cs >= 500))
    @test round(ananodate, Microsecond, RoundUp, RoundNearestTiesUp) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs + (ns >= 500))
    @test round(ananodate, Nanosecond, RoundUp, RoundNearestTiesUp) == NanoDate(yr, mn, dy, hr, mi, sc, ms, cs, ns)
end
