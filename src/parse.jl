# function Base.parse(::Type{NanoDate}, s::AbstractString, df::Dates.DateFormat=ISONanoDateFormat; localtime=false)
#     NanoDate(s, df; localtime)
# end

function Base.parse(::Type{NanoDate}, s::AbstractString, df::Dates.DateFormat=ISONanoDateFormat; localtime=false)
    isempty(s) &&
        throw(ArgumentError("Cannot parse an empty string as a NanoDate"))
    nd = tryparse(NanoDate, s, df; localtime)
    if isnothing(nd)
        throw(ArgumentError("invalid NanoDatestring"))
    end
    return nd
end

function Base.tryparse(::Type{NanoDate}, s::AbstractString, df::DateFormat=ISONanoDateFormat; localtime=false)
    try
        return NanoDate(s, df; localtime)
    catch err
        if err isa ArgumentError
            return nothing
        else
            rethrow(err)
        end
    end
end

function Base.tryparse(::Type{NanoDate}, s::AbstractString, ::typeof(ISONanoDateFormat); localtime=false)
    # Ignore leading or trailing space
    i = findfirst(!isspace, s)
    end_pos = findlast(!isspace, s)
    i === nothing || end_pos === nothing || i > end_pos && return nothing

    local dy
    dm = dd = Int64(1)
    th = tm = ts = tns = Int64(0)

    offset_hour = Int64(0)
    offset_minute = Int64(0)

    # Years
    let val = tryparsenext_base10_underscore(s, i, end_pos, 1; allow_underscore=false)
        val === nothing && @goto error
        dy, i, _ = val
        i > end_pos && @goto error
    end

    # Months
    c, i = iterate(s, i)::Tuple{Char, Int}
    c != '-' && @goto error
    i > end_pos && @goto done

    let val = tryparsenext_base10_underscore(s, i, end_pos, 1, 2; allow_underscore=false)
        val === nothing && @goto error
        dm, i, _ = val
        i > end_pos && @goto done
    end

    # Days
    c, i = iterate(s, i)::Tuple{Char, Int}
    c != '-' && @goto error
    i > end_pos && @goto done

    let val = tryparsenext_base10_underscore(s, i, end_pos, 1, 2; allow_underscore=false)
        val === nothing && @goto error
        dd, i, _ = val
        i > end_pos && @goto done
    end

    # Hours
    c, i = iterate(s, i)::Tuple{Char, Int}
    c in ('Z', '+', '-', '−') && @goto offset
    c != 'T' && c != ' ' && @goto error
    i > end_pos && @goto done

    let val = tryparsenext_base10_underscore(s, i, end_pos, 1, 2; allow_underscore=false)
        val === nothing && @goto error
        th, i, _ = val
        i > end_pos && @goto done
    end

    # Minutes
    c, i = iterate(s, i)::Tuple{Char, Int}
    c in ('Z', '+', '-', '−') && @goto offset
    c != ':' && @goto error
    i > end_pos && @goto done

    let val = tryparsenext_base10_underscore(s, i, end_pos, 1, 2; allow_underscore=false)
        val === nothing && @goto error
        tm, i, _ = val
        i > end_pos && @goto done
    end

    # Seconds
    c, i = iterate(s, i)::Tuple{Char, Int}
    c in ('Z', '+', '-', '−') && @goto offset
    c != ':' && @goto error
    i > end_pos && @goto done

    let val = tryparsenext_base10_underscore(s, i, end_pos, 1, 2; allow_underscore=false)
        val === nothing && @goto error
        ts, i, _ = val
        i > end_pos && @goto done
    end

    # Subseconds
    c, i = iterate(s, i)::Tuple{Char, Int}
    c in ('Z', '+', '-', '−') && @goto offset
    c != '.' && @goto error
    i > end_pos && @goto done

    let val = tryparsenext_base10_underscore(s, i, end_pos, 0)
        s[i] in ('Z', '+', '-', '−') && @goto offset
        val === nothing && @goto error
        tns, j, ndigits = val
        if ndigits > 9
            tns = tns÷10^(ndigits - 9)
        else
            tns *= 10 ^ (9 - ndigits)
        end
        i = j
    end

    @label offset
    localtime && @goto done
    plus_minus = Int64(0)
    seen_hours_offset = false
    seen_minutes_offset = false

    i = prevind(s, i)
    while i <= end_pos
        c = s[i]

        seen_hours_offset && seen_minutes_offset && @goto error

        if c == 'Z'
            if i == end_pos
                break
            else
                @goto error
            end
        elseif c == '-' || c == '−' # Unicode minus is allowed in ISO format
            i == end_pos && @goto error
            plus_minus = Int64(-1)
            i = nextind(s, i)
        elseif c == '+'
            i == end_pos && @goto error
            plus_minus = Int64(1)
            i += 1
        elseif '0' <= c <= '9' && iszero(plus_minus)
            # Soak up extra digits
            i += 1
        elseif '0' <= c <= '9'
            offset_hour_or_minute = tryparsenext_base10_underscore(
                s, i, end_pos, 2, 2; allow_underscore=false
            )
            isnothing(offset_hour_or_minute) && @goto error
            offset, i = offset_hour_or_minute
            if !seen_hours_offset
                offset_hour = plus_minus*offset[1]
                seen_hours_offset = true
            else
                offset_minute = plus_minus*offset[1]
                seen_minutes_offset = true
            end
        elseif c == ':'
            seen_hours_offset && i < end_pos || @goto error
            i += 1
        elseif c == '_'
            i += 1
        else
            @goto error
        end
    end


    @label done
    nanotime = NanoDate(dy, dm, dd, th, tm, ts) + Nanosecond(tns)
    if localtime
        return nanotime
    else
        return nanotime - Hour(offset_hour) - Minute(offset_minute)
    end

    @label error
    return nothing
end

# Adapted from julia/share/julia/stdlib/v1.10/Dates/src/parse.jl (Julia v1.10.4)
# License: MIT: https://julialang.org/license
"""
    tryparsenext_base10_underscore(str, i, len, min_width=1, max_width=0; allow_underscore=true) -> number, next_ind, num_digits

Like `Dates.tryparsenext_base10`, but skips over underscore characters.
"""
@inline function tryparsenext_base10_underscore(str::AbstractString, i::Int, len::Int, min_width::Int=1, max_width::Int=0; allow_underscore=true)
    i > len && return nothing
    min_pos = min_width <= 0 ? i : i + min_width - 1
    max_pos = max_width <= 0 ? len : min(i + max_width - 1, len)
    d::Int64 = 0
    ndigits_read = 0
    @inbounds while i <= max_pos
        c, ii = iterate(str, i)::Tuple{Char, Int}
        if '0' <= c <= '9'
            d = d * 10 + (c - '0')
            ndigits_read += 1
        elseif c == '_'
            if !allow_underscore
                break
            end
            # Skip over underscores otherwise
        else
            break
        end
        i = ii
    end
    if i <= min_pos
        return nothing
    else
        return d, i, ndigits_read
    end
end
