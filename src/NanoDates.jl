module NanoDates

export NanoDate,
        nnow, ntoday, date_time,
        nanodate2rata, rata2nanodate,
        nanodate2unixnanos, nanodate2unixmicros, nanodate2unixmillis, nanodate2unixseconds,
        unixnanos2nanodate, unixmicros2nanodate, unixmillis2nanodate, unixseconds2nanodate,
        format,
        CapitalT, SmallCapitalT,
        SingleSpace, Underscore,
        SmallWhiteCircle, SmallWhiteStar

using Dates
using Dates: AbstractDateTime, CompoundPeriod,
             format, value, days,
             toms, tons, UTD, UTM

include("constants.jl")
include("nanodate.jl")
include("accessors.jl")
include("conversions.jl")
include("compare.jl")
include("interop.jl")
include("arith.jl")
include("fastintmath.jl")
include("compound.jl")
include("strings.jl")
include("timestamp.jl")
include("io.jl")

@inline nanos_elapsed() = time_ns() - NanosAtStart

function __init__()
    global NanosAtStart = time_ns() 
end

end  # NanoDates

