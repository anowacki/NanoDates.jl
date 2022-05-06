var documenterSearchIndex = {"docs":
[{"location":"appropriate/advantages/#Appealing-Advantages","page":"Advantages","title":"Appealing Advantages","text":"","category":"section"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"NanoDates just work; they play well with Julia packages.","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"Are your resources more capable than DateTime understands?\nDo you produce, consume, or otherwise ferry microtimed occurances?\nIs your masterful realtime design standing by as the nanoseconds fly?","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"NanoDates are your fuller realization of dates-with-times.\nEach NanoDate is constructed into the temporal context given.\nthat makes them strongly continumm localic \n\"near\" and \"far\" become as here and there","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"In 2018 finanical centers, through their host countries, adopted regulations that apply to organizations and individuals participating in the high frequency  trade-by-trade flow that gives rise to market microdynamics. Here are two quotes from the 2018 regulators:","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"Market events and order transactions must be recorded  [and] retraceable to UTC.","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"Systems that are syncronized to a [validated network] clock, require timestamp availability at submillisecond resolutions. The shortest interval that is required of very high frequncy trading work is 25ns. ","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"","category":"page"},{"location":"appropriate/advantages/","page":"Advantages","title":"Advantages","text":"UTC is the timebase of record for most people as convienice and for statuory puposes.","category":"page"},{"location":"use/construction/#Constructing-NanoDates","page":"Construction","title":"Constructing NanoDates","text":"","category":"section"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"All of the ways available to construct DateTimes work with NanoDates.  And there are a few additional constructors that make work microseconds and nanoseconds easier.  NanoDate constructors are best understood by example. We will set some variables for later clarity.","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"using Dates, NanoDates\n\n# lets get the parts from Dates\n\nyears, months, days = (2022, 4, 28);\nhours, mins, secs, millis, micros, nanos = \n  (14, 32, 10, 123, 456, 789);\n\ndayt = Date(years, months, days)\n# 2022-04-28\n\ntyme = Time(hours, mins, secs, millis, micros, nanos)\n# 14:32:10.123456789\n\n# to get a DateTime from a Date and a Time safely\n# > tyme_ms = trunc(tyme, Millisecond)\n# > daytime = DateTime(dayt, tyme_ms)\n# NanoDates exports `date_time` that does this\n\ndaytime = date_time(dayt, tyme)\n# 2022-04-28T14:32:10.123","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"Here are familiar constructor methods.","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"nd = NanoDate(dayt)\n# 2022-04-28T00:00:00\n\nnd = NanoDate(daytime)\n# 2022-04-28T14:32:10.123\n\nnd = NanoDate(dayt, tyme)\n# 2022-04-28T14:32:10.123456789","category":"page"},{"location":"use/intostring/#Converting-NanoDates-and-Strings","page":"Strings","title":"Converting NanoDates and Strings","text":"","category":"section"},{"location":"use/intostring/","page":"Strings","title":"Strings","text":"The easiest way to obtain your NanoDate as a String is to use string(nanodate). More easily readable is string(nanodate; sep='<choose a Char>' ).","category":"page"},{"location":"use/intostring/","page":"Strings","title":"Strings","text":"using Dates, NanoDates\n\nnd = NanoDate(DateTime(\"2022-07-28T12:15:30.118\"),\n              Nanosecond(375852));\n\nstring(nd)\n\"2022-07-28T12:15:30.118375852\"\n\nstring(nd; sep='_')\n\"2022-07-28T12:15:30.118_375_852\"\n\nstring(nd; sep='◦')\n\"2022-07-28T12:15:30.118◦375◦852\"\n\nstring(nd; sep='∙')\n\"2022-07-28T12:15:30.118∙375∙852\"","category":"page"},{"location":"use/specify/#Construction-by-Specification","page":"Specification","title":"Construction by Specification","text":"","category":"section"},{"location":"use/specify/","page":"Specification","title":"Specification","text":"Here are some of the methods that take values","category":"page"},{"location":"use/specify/","page":"Specification","title":"Specification","text":"nd = NanoDate(years)\n# 2022-01-01T00:00:00\n\nnd = NanoDate(years, months)\n# 2022-04-01T00:00:00\n\nnd = NanoDate(years, months, days, \n              hours, mins, secs)\n# 2022-04-28T14:32:10\n\nnd = NanoDate(years, months, days, \n              hours, mins, secs,\n              millis, micros, nanos)\n# 2022-04-28T14:32:10.123456789","category":"page"},{"location":"use/specify/","page":"Specification","title":"Specification","text":"And some of the methods that take periods","category":"page"},{"location":"use/specify/","page":"Specification","title":"Specification","text":"nd = NanoDate(Year(years))\n# 2022-01-01T00:00:00\n\nnd = NanoDate(Year(years), Month(months))\n# 2022-04-01T00:00:00\n\nnd = NanoDate(\n    Year(years), Month(months), Day(days), \n    Hour(hours), Minute(mins), Second(secs))\n# 2022-04-28T14:32:10\n\nnd = NanoDate(\n    Year(years), Month(months), Day(days), \n    Hour(hours), Minute(mins), Second(secs),\n    Millisecond(millis), Microsecond(micros),\n    Nanosecond(nanos))\n# 2022-04-28T14:32:10.123456789","category":"page"},{"location":"use/convenient/#Convenient-Work-Alikes","page":"Conviences","title":"Convenient Work-Alikes","text":"","category":"section"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"Here are a few simple timesavers, DateTime work-alikes. nnow(), nnow(UTC) are similar to now(), now(UTC), with support for specifying microseconds and nanoseconds.","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"# nnow(), nnow(UTC) work like now(), now(UTC)\n\nnow()                        # 1 millisecond resolution\n# 2022-04-25T10:09:40.094\n\nnnow()                       # 100 nanosecond resolution (ymmv)\n# 2022-04-25T10:09:40.094615300","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"These additional forms are available.","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"nnow(Microsecond(cs)), nnow(UTC, Microsecond(ns)),\nnnow(Nanosecond(cs)),  nnow(UTC, Nanosecond(ns))\n\nnnow(Microsecond(cs), Nanosecond(ns)),\nnnow(UTC, Microsecond(cs), Nanosecond(ns))","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"ntoday() and ntoday(UTC) are provided. They work like today(), adding UTC.","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"today()\n# 2022-04-25\n\nntoday(), ntoday(UTC)\n# 2022-04-25, 2022-04-26","category":"page"},{"location":"technical/DatesFunctions/#Getting-Integer-Valued-Information","page":"-","title":"Getting Integer Valued Information","text":"","category":"section"},{"location":"technical/DatesFunctions/","page":"-","title":"-","text":"","category":"page"},{"location":"technical/DatesFunctions/#low-level-internals-from-Dates","page":"-","title":"low level internals from Dates","text":"","category":"section"},{"location":"technical/DatesFunctions/","page":"-","title":"-","text":"function mapping\nvalue(x::T): x –> integer value underlying T\ntoms(x::Period): x –> milliseconds\ntons(x::Period): x –> nanoseconds\n \ndays(x::DatePeriod): x –>  truncates (rounds down, floors)\n ::Nanoseconds .. ::Hour, ::Day, ::Week\n ::Date, ::DateTime –> daycount (RataDie)","category":"page"},{"location":"technical/DatesFunctions/","page":"-","title":"-","text":"","category":"page"},{"location":"betterinformed/their_dual_natures/#NanoDates:-time-from-the-inside","page":"Inside  Time","title":"NanoDates: time from the inside","text":"","category":"section"},{"location":"betterinformed/tablescraps/b/#Temporal-periods,-two-ways","page":"-","title":"Temporal periods, two ways","text":"","category":"section"},{"location":"betterinformed/tablescraps/b/","page":"-","title":"-","text":"All of the TimePeriod and the DatePeriod types are found in two distinct contexts. They are most familiar participating in specific clock and calendar designations.","category":"page"},{"location":"betterinformed/tablescraps/b/","page":"-","title":"-","text":"\"Next year, school starts on Jan 4 at 9:15.\"","category":"page"},{"location":"betterinformed/tablescraps/b/","page":"-","title":"-","text":"That way of working with temporal periods relies on the attachment of periods to eventualities. Their other role occurs in a creative, rather than a descriptive context.  Periods exent, available and able, unfettered, nonassociated.","category":"page"},{"location":"betterinformed/tablescraps/b/","page":"-","title":"-","text":"\"It takes me several hours to measure an attosecond.\"","category":"page"},{"location":"betterinformed/tablescraps/b/","page":"-","title":"-","text":"That is the justification for holding extra nanoseconds.","category":"page"},{"location":"betterinformed/tablescraps/b/","page":"-","title":"-","text":"","category":"page"},{"location":"betterinformed/tablescraps/b/","page":"-","title":"-","text":"clock and calendar is thier presence and utilization As free periods, periods not attached to a specific Time or any given Date, Nanoseconds are allowed to hold type to hold quantities exceeding their own field rollover count (the next nanosecond after the 999th carries all 1_000 of them into a single microsecond). And that lets us keep our struct to two fields while covering all Days of each Year one Nanosecond at a time.","category":"page"},{"location":"use/compoundperiod/#CompoundPeriods-and-Period-Compounding","page":"Period Compounding","title":"CompoundPeriods and Period Compounding","text":"","category":"section"},{"location":"use/compoundperiod/","page":"Period Compounding","title":"Period Compounding","text":"The primary Time, Date, and Date-with-Time types are individually seperable into and reintegrable from appropriately constituted CompoundPeriods.","category":"page"},{"location":"use/compoundperiod/","page":"Period Compounding","title":"Period Compounding","text":"CompoundPeriod is a type internal to Dates that is the construct obtained when two (or more) distinct DatePeriods aor TimePeriods are additively combinded.","category":"page"},{"location":"use/compoundperiod/","page":"Period Compounding","title":"Period Compounding","text":"using Dates, NanoDates\nusing Dates: CompoundPeriod","category":"page"},{"location":"use/compoundperiod/#note-these-are-given-smallest-period-..-largest","page":"Period Compounding","title":"note these are given smallest period .. largest","text":"","category":"section"},{"location":"use/compoundperiod/","page":"Period Compounding","title":"Period Compounding","text":"const TimeStampPeriods = \n    (Nanosecond, Microsecond, Millsecond, Second, Minute, Hour)\nconst DateStampPeriods = (:Day, ::Month, :Year)\n\nconst TimeStampPeriods = \n    (Nanosecond, Microsecond, Millsecond, Second, Minute, :Hour)\nconst DateLikePeriods  = (:Day, :Week, :Month, :Quarter, :Year)\n\nconst NanoStampPeriods =\n    (Nanosecond, Microsecond, Millsecond, Second, Minute,:Hour,\n     :Day, :Week, :Month, :Quarter, :Year)","category":"page"},{"location":"use/compoundperiod/#nothing-to-see,-nothing-to-here","page":"Period Compounding","title":"nothing to see, nothing to here","text":"","category":"section"},{"location":"use/compoundperiod/","page":"Period Compounding","title":"Period Compounding","text":"date = Date(2022, 6, 3)   # 2022 years, 6 months, 3 days  \nCompoundPeriod(date)      # 2022 years, 6 months, 3 days\nDate(ans)                 # 2022-05-03\n\ncompound_date = CompoundPeriod(Date(\"2022-05-03\")","category":"page"},{"location":"betterinformed/the_representation/#Dates-and-Nanoseconds","page":"Outside Time","title":"Dates and Nanoseconds","text":"","category":"section"},{"location":"betterinformed/the_representation/#The-Representation","page":"Outside Time","title":"The Representation","text":"","category":"section"},{"location":"betterinformed/the_representation/#NanoDates-are-structs-with-two-fields,","page":"Outside Time","title":"NanoDates are structs with two fields,","text":"","category":"section"},{"location":"betterinformed/the_representation/","page":"Outside Time","title":"Outside Time","text":"One holds a value of type DateTime (containing Year, .., Millisecond periods).","category":"page"},{"location":"betterinformed/the_representation/","page":"Outside Time","title":"Outside Time","text":"The other holds submillisecond information (containing Microseconds and Nanoseconds). This information is kept as a quantity of Nanoseconds, a quantity in 0..999999. So, any Microseconds, and there may be none or as many as 999 (no more though), are converted to their equivalant  duration in Nanoseconds (1000 each) and the totality of time spanned is remains unchanged. These duration balancing Nanoseconds are added to any that are present already.","category":"page"},{"location":"betterinformed/the_representation/","page":"Outside Time","title":"Outside Time","text":"Being clock and calendar is their presence and the manner of their utilization. As free periods, periods not attached to a specific Time or any given Date, Nanoseconds are allowed to hold type to hold quantities exceeding their own field rollover count (the next nanosecond after the 999th carries all 1_000 of them into a single microsecond). And that lets us keep our struct to two fields while covering all Days of each Year one Nanosecond at a time.","category":"page"},{"location":"use/accessors/#Getting-Parts-of-a-NanoDate","page":"Accessors","title":"Getting Parts of a NanoDate","text":"","category":"section"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"This works just the way it does with Date, Time, and DateTime.","category":"page"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"You can use year, month, day, .., nanosecond You can use Year, Month, Day, ... Nanosecond","category":"page"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"using Dates, NanoDates\n\ndayt = Date(\"2022-04-28\")\ntyme = Time(14, 32, 10, 123, 456, 789)\nnanodate = NanoDate(dayt, tyme)\n\nyear(nanodate) == 2022\nday(nanodate) == 10\nhour(nanodate) == 14\nnanosecond(nanodate) == 789\n\nYear(nanodate) == Year(2022)\nDay(nanodate) == Day(10)\nHour(nanodate) == Hour(14)\nNanosecond(nanodate) == Nanosecond(789)","category":"page"},{"location":"use/accessors/#Adding/Subtracting-Periods","page":"Accessors","title":"Adding/Subtracting Periods","text":"","category":"section"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"This works just the way it does with Date, Time, and DateTime.","category":"page"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"julia> nanodate\n2022-04-28T14:32:10.123456789\n\njulia> nanodate + Month(2)\n2022-06-28T14:32:10.123456789\n\njulia> nanodate + Month(2) + Second(35)\n2022-06-28T14:32:45.123456789\n\njulia> nanodate - Month(4)\n2021-12-28T14:32:10.123456789\n\njulia> nanodate - Month(4) - Hour(14)\n2021-12-28T00:32:10.123456789\n\njulia> nanodate - Month(4) - Hour(15)\n2021-12-27T23:32:10.123456789","category":"page"},{"location":"#NanoDates.jl","page":"Home","title":"NanoDates.jl","text":"","category":"section"},{"location":"#A-date-and-time-type-with","page":"Home","title":"A date-and-time type with","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Years, Months, Days\nHours, Minutes, Seconds\nMilliseconds, Microseconds,\nNanoseconds","category":"page"},{"location":"#plays-well-with-others","page":"Home","title":"plays well with others","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"NanoDate works like DateTime with more precision\nSupports Dates.jl methods\nInterconverts with DateTime, Date","category":"page"},{"location":"#introduces-enhancements","page":"Home","title":"introduces enhancements","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"supports splicing in period values","category":"page"},{"location":"","page":"Home","title":"Home","text":"- NanoDate(nd, Hour(5))\n   - replaces Hour(nd) with Hour(5)\n- NanoDate(nd, Week(2))\n   - moves to Week(2) of the Year(nd)","category":"page"},{"location":"","page":"Home","title":"Home","text":"option to separate subseconds","category":"page"},{"location":"","page":"Home","title":"Home","text":"   - 2022-04-28T02:15:30.124_455_831\n   - 2022-04-28T02:15:30.124◦455◦831","category":"page"},{"location":"","page":"Home","title":"Home","text":"","category":"page"},{"location":"","page":"Home","title":"Home","text":"installs with  using Pkg; Pkg.add(\"NanoDates\")\nask questions\non Discourse\non Slack\non Zulip – using the stream dates-times-zones\nreport any issues here","category":"page"},{"location":"","page":"Home","title":"Home","text":"","category":"page"}]
}