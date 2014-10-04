-module(helper).
-compile(export_all).

trillium_connect() ->
    erlang:set_cookie(node(), 'erlang-rocks'),
    net_kernel:connect('pi@192.168.2.2').
