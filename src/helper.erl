-module(helper).
-compile(export_all).

trillium_register() ->
    erlang:set_cookie(node(), 'erlang-rocks'),
    io:format("register(trillium, self()).~n"),
    register(trillium, self()).

trillium_connect() ->
    io:format("net_kernel:connect('pi@192.168.2.2').~n"),
    net_kernel:connect('pi@192.168.2.2').

send_to_trillium(Msg) ->
    io:format("{trillium, 'pi@192.168.2.1'} ! ~p.~n", [Msg]),
    {trillium, 'pi@192.168.2.1'} ! Msg.

msg(Name) ->
    io:format("display:show_welcome_message(~p).~n", [Name]),
    display:show_welcome_message(Name).

get_pi_mac_from_devices(Devices) ->
    [Dev] = [Dev0 || "00:02:72" ++ _Rest = Dev0 <- Devices],
    {ok, Dev}.

bt_discover_pi() ->
    io:format("{ok, Devs} = bluetooth_interface:discover(),~n"),
    {ok, Devs} = bluetooth_interface:discover(),
    io:format("helper:get_pi_mac_from_devices(~p).~n", [Devs]),
    get_pi_mac_from_devices(Devs).

bt_name(Mac) ->
    io:format("bluetooth_interface:get_remote_name(~p).~n", [Mac]),
    bluetooth_interface:get_remote_name(Mac).

get_bt_name_and_send(Mac) ->
    {ok, Name} = bt_name(Mac),
    send_to_trillium({name, Name}).
