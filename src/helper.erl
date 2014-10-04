-module(helper).
-compile(export_all).

trillium_connect() ->
    erlang:set_cookie(node(), 'erlang-rocks'),
    io:format("net_kernel:connect('pi@192.168.2.2').~n"),
    net_kernel:connect('pi@192.168.2.2').

msg(Name) ->
    io:format("display:show_welcome_message(~p).~n", [Name]),
    display:show_welcome_message(Name).
