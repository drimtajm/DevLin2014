-module(helper).
-compile(export_all).

trillium_connect() ->
    erlang:set_cookie(node(), 'erlang-rocks'),
    io:format("global:register_name(trillium, self()),~n"),
    global:register_name(trillium, self()),
    io:format("net_kernel:connect('pi@192.168.2.2').~n"),
    net_kernel:connect('pi@192.168.2.2').

msg(Name) ->
    io:format("display:show_welcome_message(~p).~n", [Name]),
    display:show_welcome_message(Name).

bt_name(Mac) ->
    io:format("bluetooth_interface:get_remote_name(~p).~n", [Mac]),
    bluetooth_interface:get_remote_name(Mac).

