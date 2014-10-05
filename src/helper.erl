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
    {ok, Devs0} = bluetooth_interface:discover(),
    Devs = case Devs0 of
	       []    -> ["00:02:72:c0:64:11"];
	       _Else -> Devs0
	   end,
    io:format("helper:get_pi_mac_from_devices(~p).~n", [Devs]),
    get_pi_mac_from_devices(Devs).

bt_name(Mac) ->
    io:format("bluetooth_interface:get_remote_name(~p).~n", [Mac]),
    Name0 = bluetooth_interface:get_remote_name(Mac),
    case Name0 of
	unknown -> "datorbebis";
	_Else   -> Name0
    end.

get_bt_name_and_send(Mac) ->
    {ok, Name} = bt_name(Mac),
    send_to_trillium({name, Name}).

change_bt_names(Names, Interval) ->
    lists:foldl(fun(Name, N) ->
		    case N rem Interval of
			0 -> bluetooth_interface:set_local_name(Name),
			     timer:sleep(1500);
			_ -> ok
		    end,
		    N+1
		end, 0, Names).

get_bt_names_in_background(Mac) ->
    Fun = fun() ->
	    {ok, Name} = bluetooth_interface:get_remote_name(Mac),
	    case Name of
		unknown -> ok;
	        _Else   ->
		    {recursion, 'pi@192.168.2.1'} ! {name, Name},
		    timer:sleep(1500)
	    end
	  end,
    recursion(Fun).

receive_bt_names_in_background() ->
    Fun = fun() ->
	    receive {name, Name} ->
		    display:show_welcome_message(Name)
	    end
	  end,
    recursion(Fun).

recursion(Fun) ->
    spawn(fun() ->
		  register(recursion, self()),
		  recursion_helper(Fun)
	  end).

recursion_helper(Fun) ->
    receive
	stop -> ok
    after 0 ->
	    Fun(),
	    recursion_helper(Fun)
    end.		    

phone_mac() ->
    "98:0D:2E:0F:73:13".

phone_name() ->
    "4stone".
