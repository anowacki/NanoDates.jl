var documenterSearchIndex = {"docs":
[{"location":"use/construction/#Constructing-NanoDates","page":"Construction","title":"Constructing NanoDates","text":"","category":"section"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"All of the ways available to construct DateTimes work with NanoDates.  And there are a few additional constructors that make work microseconds and nanoseconds easier.  NanoDate constructors are best understood by example. We will set some variables for later clarity.","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"using Dates, NanoDates\n\n# lets get the parts from Dates\n\nyears, months, days = (2022, 4, 28);\nhours, mins, secs, millis, micros, nanos = \n  (14, 32, 10, 123, 456, 789);\n\ndayt = Date(years, months, days)\n# 2022-04-28\n\ntyme = Time(hours, mins, secs, millis, micros, nanos)\n# 14:32:10.123456789\n\n# to get a DateTime from a Date and a Time safely\n# > tyme_ms = trunc(tyme, Millisecond)\n# > daytime = DateTime(dayt, tyme_ms)\n# NanoDates exports `date_time` that does this\n\ndaytime = date_time(dayt, tyme)\n# 2022-04-28T14:32:10.123","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"Here are familiar constructor methods.","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"nd = NanoDate(dayt)\n# 2022-04-28T00:00:00\n\nnd = NanoDate(daytime)\n# 2022-04-28T14:32:10.123\n\nnd = NanoDate(dayt, tyme)\n# 2022-04-28T14:32:10.123456789","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"Here are some of the methods that take values","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"nd = NanoDate(years)\n# 2022-01-01T00:00:00\n\nnd = NanoDate(years, months)\n# 2022-04-01T00:00:00\n\nnd = NanoDate(years, months, days, \n              hours, mins, secs)\n# 2022-04-28T14:32:10\n\nnd = NanoDate(years, months, days, \n              hours, mins, secs,\n              millis, micros, nanos)\n# 2022-04-28T14:32:10.123456789","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"And some of the methods that take periods","category":"page"},{"location":"use/construction/","page":"Construction","title":"Construction","text":"nd = NanoDate(Year(years))\n# 2022-01-01T00:00:00\n\nnd = NanoDate(Year(years), Month(months))\n# 2022-04-01T00:00:00\n\nnd = NanoDate(\n    Year(years), Month(months), Day(days), \n    Hour(hours), Minute(mins), Second(secs))\n# 2022-04-28T14:32:10\n\nnd = NanoDate(\n    Year(years), Month(months), Day(days), \n    Hour(hours), Minute(mins), Second(secs),\n    Millisecond(millis), Microsecond(micros),\n    Nanosecond(nanos))\n# 2022-04-28T14:32:10.123456789","category":"page"},{"location":"use/intostring/#Converting-NanoDates-to-Strings","page":"Strings","title":"Converting NanoDates to Strings","text":"","category":"section"},{"location":"use/intostring/","page":"Strings","title":"Strings","text":"The easiest way to obtain your NanoDate as a String is to use string(nanodate). Similarly easy, and more readable is string(nanodate; sep='<choose a Char>' ).","category":"page"},{"location":"use/intostring/","page":"Strings","title":"Strings","text":"using Dates, NanoDates\n\nnd = NanoDate(DateTime(\"2022-07-28T12:15:30.118\"),\n              Nanosecond(375852));\n\nstring(nd)\n\"2022-07-28T12:15:30.118375852\"\n\nstring(nd; sep='_')\n\"2022-07-28T12:15:30.118_375_852\"\n\nstring(nd; sep='◦')\n\"2022-07-28T12:15:30.118◦375◦852\"\n\nstring(nd; sep='∙')\n\"2022-07-28T12:15:30.118∙375∙852\"","category":"page"},{"location":"use/accessors/#Getting-Parts-of-a-NanoDate","page":"Accessors","title":"Getting Parts of a NanoDate","text":"","category":"section"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"This works just the way it does with Date, Time, and DateTime.","category":"page"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"You can use year, month, day, .., nanosecond You can use Year, Month, Day, ... Nanosecond","category":"page"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"using Dates, NanoDates\n\ndayt = Date(\"2022-04-28\")\ntyme = Time(14, 32, 10, 123, 456, 789)\nnanodate = NanoDate(dayt, tyme)\n\nyear(nanodate) == 2022\nday(nanodate) == 10\nhour(nanodate) == 14\nnanosecond(nanodate) == 789\n\nYear(nanodate) == Year(2022)\nDay(nanodate) == Day(10)\nHour(nanodate) == Hour(14)\nNanosecond(nanodate) == Nanosecond(789)","category":"page"},{"location":"use/accessors/#Adding/Subtracting-Periods","page":"Accessors","title":"Adding/Subtracting Periods","text":"","category":"section"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"This works just the way it does with Date, Time, and DateTime.","category":"page"},{"location":"use/accessors/","page":"Accessors","title":"Accessors","text":"julia> nanodate\n2022-04-28T14:32:10.123456789\n\njulia> nanodate + Month(2)\n2022-06-28T14:32:10.123456789\n\njulia> nanodate + Month(2) + Second(35)\n2022-06-28T14:32:45.123456789\n\njulia> nanodate - Month(4)\n2021-12-28T14:32:10.123456789\n\njulia> nanodate - Month(4) - Hour(14)\n2021-12-28T00:32:10.123456789\n\njulia> nanodate - Month(4) - Hour(15)\n2021-12-27T23:32:10.123456789","category":"page"},{"location":"use/convenient/#Convenient-Work-Alikes","page":"Conviences","title":"Convenient Work-Alikes","text":"","category":"section"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"Here are a few simple timesavers, DateTime work-alikes. nnow(), nnow(UTC) are similar to now(), now(UTC), with support for specifying microseconds and nanoseconds.","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"# nnow(), nnow(UTC) work like now(), now(UTC)\n\nnow()                        # 1 millisecond resolution\n# 2022-04-25T10:09:40.094\n\nnnow()                       # 100 nanosecond resolution (ymmv)\n# 2022-04-25T10:09:40.094615300","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"These additional forms are available.","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"nnow(Microsecond(cs)), nnow(UTC, Microsecond(ns)),\nnnow(Nanosecond(cs)),  nnow(UTC, Nanosecond(ns))\n\nnnow(Microsecond(cs), Nanosecond(ns)),\nnnow(UTC, Microsecond(cs), Nanosecond(ns))","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"ntoday() and ntoday(UTC) are provided. They work like today(), adding UTC.","category":"page"},{"location":"use/convenient/","page":"Conviences","title":"Conviences","text":"today()\n# 2022-04-25\n\nntoday(), ntoday(UTC)\n# 2022-04-25, 2022-04-26","category":"page"},{"location":"#NanoDates.jl","page":"Home","title":"NanoDates.jl","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Offers NanoDate, a date-and-time type with nanosecond resolution","category":"page"},{"location":"#plays-well-with-others","page":"Home","title":"plays well with others","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"NanoDate works like DateTime with more precision\nSupports Dates.jl methods\nInterconverts with DateTime, Date","category":"page"},{"location":"#introduces-enhancements","page":"Home","title":"introduces enhancements","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"supports splicing in period values","category":"page"},{"location":"","page":"Home","title":"Home","text":"    - NanoDate(nd, Hour(5))\n      - replaces Hour(nd) with Hour(5)\n    - NanoDate(nd, Week(2))\n      - moves to Week(2) of the Year(nd)","category":"page"},{"location":"","page":"Home","title":"Home","text":"option to separate subseconds","category":"page"},{"location":"","page":"Home","title":"Home","text":"    - 2022-04-28T02:15:30.124_455_831\n    - 2022-04-28T02:15:30.124◦455◦831","category":"page"},{"location":"","page":"Home","title":"Home","text":"","category":"page"},{"location":"","page":"Home","title":"Home","text":"installs with  using Pkg; Pkg.add(\"NanoDates\")\nask questions\non Discourse\non Slack\non Zulip – using the stream dates-times-zones\nreport any issues here","category":"page"},{"location":"","page":"Home","title":"Home","text":"","category":"page"},{"location":"technical/DatesFunctions/#Getting-Integer-Valued-Information","page":"-","title":"Getting Integer Valued Information","text":"","category":"section"},{"location":"technical/DatesFunctions/","page":"-","title":"-","text":"","category":"page"},{"location":"technical/DatesFunctions/#low-level-internals-from-Dates","page":"-","title":"low level internals from Dates","text":"","category":"section"},{"location":"technical/DatesFunctions/","page":"-","title":"-","text":"function mapping\nvalue(x::T): x –> integer value underlying T\ntoms(x::Period): x –> milliseconds\ntons(x::Period): x –> nanoseconds\n \ndays(x::DatePeriod): x –>  truncates (rounds down, floors)\n ::Nanoseconds .. ::Hour, ::Day, ::Week\n ::Date, ::DateTime –> daycount (RataDie)","category":"page"},{"location":"technical/DatesFunctions/","page":"-","title":"-","text":"","category":"page"}]
}
