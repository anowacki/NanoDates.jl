module NanoDates

export NanoDate
export nanodate2rata, rata2nanodate, nanodate2unix, unix2nanodate

using Dates
using Dates: AbstractDateTime, CompoundPeriod,
                value, days, toms, tons

include("constants.jl")
include("nanodate.jl")
include("accessors.jl")
include("conversions.jl")
include("compare.jl")
include("interop.jl")
include("arith.jl")
include("strings.jl")

end

