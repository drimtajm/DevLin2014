%%% @author  drimtajm
%%% @copyright (C) 2014, 
%%% @doc
%%% Query the SCB REST API
%%% @end
%%% Created : 20 Sep 2014 by drimtajm

-module(scb_client).
-export([fetch_names_from_statistics/0]).

-define(URI, "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0001/BE0001T04BAr").

fetch_names_from_statistics() ->
    application:start(inets),
    {ok, {_Status, _Headers, Body}} =
	httpc:request(get, {?URI, [{"Accept", "application/json"},
				   {"Accept-Charset", "utf-8"},
				   {"Accept-Language", "sv_SE"}]}, [], []),
    DecodedBody = jsx:decode(list_to_binary(Body)),
    Variables = proplists:get_value(<<"variables">>, DecodedBody),
    Values = proplists:get_value(<<"valueTexts">>, hd(Variables)),
    {ok, lists:map(fun unicode:characters_to_list/1, Values)}.
