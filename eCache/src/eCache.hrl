%%%-------------------------------------------------------------------
%%% @author andy
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2014 10:11 AM
%%%-------------------------------------------------------------------
-author("andy").

%% Cached record
-record(cache_record, {k, v}).

%% Timeout for call
-define(CALL_TIMEOUT,2000).

%% Define for debug
-define(LOG(_MSG), (begin
                     io:format(" ***** ~p ~p ~p *****~n", [?MODULE, ?LINE, _MSG])
                   end)).
