elyskom
=====

An OTP library for communicating with LysKOM servers. See https://www.lysator.liu.se/lyskom/index_en.html for more about what that is.

Build
-----

    $ rebar3 compile

Usage
-----

    1> {ok, Pid} = elyskom:new().
    {ok,<0.238.0>}
    2> elyskom:get_time(Pid).
    {{2021,6,25},{10,59,24}}
    3> 

All the calls listed as recommended in the Protocol A specification are implemented. The data types are somewhat simplified, but that shouldn't be very noticeable from Erlang. Run `rebar3 edoc` to build the documentation. It won't make much sense without the Protocol A spec, but you really should read that one anyway if you're going to be writing any sort of LysKOM client software.