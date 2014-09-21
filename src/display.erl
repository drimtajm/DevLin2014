%%%-------------------------------------------------------------------
%%% @author  <pi@datorbebis>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2014 by  <pi@datorbebis>
%%%-------------------------------------------------------------------
-module(display).

-behaviour(gen_server).

%% API
-export([start_link/0, show_welcome_message/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(lcdRs, 7).
-define(lcdE, 8).
-define(lcdD4, 17).
-define(lcdD5, 18).
-define(lcdD6, 21).
-define(lcdD7, 22).
-define(pins, [?lcdRs, ?lcdE, ?lcdD4, ?lcdD5, ?lcdD6, ?lcdD7]). 
-define(clearScreenCmd, 1).
-define(setCursorCmd, 16#80).

-define(cmd_mode, 0).
-define(char_mode, 1).

-define(call(Cmd), gen_server:call({global, ?SERVER}, Cmd)).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop() ->
    ?call(stop).

show_welcome_message(Name) ->
    ?call({show_welcome_message, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    open_and_configure_pins(),
    send_command(clearscreen),
    {ok, #state{}}.

handle_call({show_welcome_message, Name}, _From, State) ->
    display_string(unicode_decode("Välkommen,\n")++Name++"!"),
    erlang:send_after(3000, self(), clearscreen),
    Reply = ok,
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(clearscreen, State) ->
    send_command(clearscreen),
    {noreply, State}.

terminate(_Reason, _State) ->
    send_command(clearscreen),
    close_pins(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

display_string([]) ->
    ok;
display_string("\n" ++ Rest) ->
    send_command({goto_line, 2}),
    display_string(Rest);
display_string(String) when is_list(String) ->
    send_char(hd(String)),
    display_string(tl(String)).

open_and_configure_pins() ->
    lists:map(fun gpio:open_pin/1, ?pins),
    lists:map(fun gpio:set_output/1, ?pins).

close_pins() ->
    lists:map(fun gpio:close_pin/1, ?pins).

send_char(228) ->
    send(16#E1, ?char_mode);
send_char(Char) when Char >= 32, Char =< 127 ->
    send(Char, ?char_mode).

send_command(Cmd) ->
    case Cmd of
	{goto_line, Nr} when Nr =:= 1 orelse Nr =:= 2 ->
	    send(16#40*(Nr+1), ?cmd_mode);
	clearscreen ->
	    send(1, ?cmd_mode);
	init ->
	    send(16#33, ?cmd_mode),
	    send(16#32, ?cmd_mode),
	    send(16#28, ?cmd_mode),
	    send(6, ?cmd_mode),
	    send(1, ?cmd_mode)
    end.

send(Data, Mode) ->
    gpio:write_pin(?lcdRs, Mode),
    <<FirstBit:1, SecondBit:1, ThirdBit:1, FourthBit:1,
      FifthBit:1, SixthBit:1, SeventhBit:1, EighthBit:1>> = <<Data>>,
    gpio:write_pin(?lcdD4, FourthBit),
    gpio:write_pin(?lcdD5, ThirdBit),
    gpio:write_pin(?lcdD6, SecondBit),
    gpio:write_pin(?lcdD7, FirstBit),
    pulse_enable(),
    gpio:write_pin(?lcdD4, EighthBit),
    gpio:write_pin(?lcdD5, SeventhBit),
    gpio:write_pin(?lcdD6, SixthBit),
    gpio:write_pin(?lcdD7, FifthBit),
    pulse_enable().

pulse_enable() ->
    gpio:set_pin_high(?lcdE),
    gpio:set_pin_low(?lcdE).

unicode_decode(String) ->
    unicode:characters_to_list(list_to_binary(String)).
