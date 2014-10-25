%%%-------------------------------------------------------------------
%%% @author andy
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2014 3:13 PM
%%%-------------------------------------------------------------------
-module(eCache).
-author("andy").

-behaviour(gen_server).

%% API
-export([count/0, start/0, put/2, get/1, start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_COUNT,3).

-record(state, {tid, keys, max_count, current_count}).

-include("eCache.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ?LOG("eCache_ets_server inited!"),
  process_flag(trap_exit, true),
  {yes, _ , Tid} = start(),
  {ok, #state{ tid = Tid, keys = queue:new(), max_count = ?MAX_COUNT}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% For put
%%
%% @end
%%--------------------------------------------------------------------
handle_call({put,#cache_record{k  =   _K,
                               v  =   _V}},
             _From,
            _State = #state { tid           =   Tid,
                              keys          =   Keys,
                              max_count     =   _MAX_COUNT,
                              current_count =   _CURRENT_COUNT}) ->
  if
    _CURRENT_COUNT >=  ?MAX_COUNT  ->
      case queue:out(Keys) of
        {{value, _OutK}, _NewKeys}  ->
            ets:delete(Tid, _OutK),
            _NewState = #state{ keys          =   _NewKeys,
                                current_count =   queue:len(_NewKeys)},
            ets:insert(Tid, { _K, _V }),
            _NewNewKeys = queue:in(_K, _NewState#state.keys),
            {reply, ok, #state{
                              tid           = Tid,
                              keys          =   _NewNewKeys,
                              max_count     =   _MAX_COUNT,
                              current_count =   queue:len(_NewNewKeys)
                              }};
        {empty, _} ->
            ets:insert(Tid, { _K, _V }),
            _NewKeys = queue:in(_K, _State#state.keys),
            {reply, ok, #state{
                               tid           = Tid,
                               keys            =   _NewKeys,
                               max_count     =   _MAX_COUNT,
                               current_count   =   queue:len(_NewKeys)}}
      end;
    _CURRENT_COUNT <  ?MAX_COUNT  ->
      ets:insert(Tid, { _K, _V }),
      _NewKeys = queue:in(_K, _State#state.keys),
      {reply, ok, #state{
        tid           = Tid,
        keys            =   _NewKeys,
        max_count     =   _MAX_COUNT,
        current_count   =   queue:len(_NewKeys)}}
  end;
%%--------------------------------------------------------------------
%% @private
%% @doc
%% For get
%%
%% @end
%%--------------------------------------------------------------------
handle_call({get,
            #cache_record { k = _K } },
            _From,
            State = #state { tid  = Tid }) ->
    {reply, ets:lookup(Tid , _K), State};
%%--------------------------------------------------------------------
%% @private
%% @doc
%% For count
%%
%% @end
%%--------------------------------------------------------------------
handle_call({count},
            _From,
            _State = #state{ keys = Keys})  ->
    {reply, queue:len(Keys), _State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Start with create an ets table and return its id
%%
%% @spec start() -> {yes, ?MODULE, Tid}.
%% @end
%%--------------------------------------------------------------------
start() ->
  Tid = ets:new(cache_record_tbl, [set]),
  {yes, ?MODULE, Tid}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Put a K-V into eCache
%%
%% @spec put(_K :: term(), _V :: term()) -> Any.
%% @end
%%--------------------------------------------------------------------
put(_K, _V)  ->
  gen_server:call(?MODULE, {put, #cache_record { k = _K, v = _V }}, ?CALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get _V from eCache by _K
%%
%% @spec get(_K :: term()) -> Any.
%% @end
%%--------------------------------------------------------------------
get(_K)  ->
  gen_server:call(?MODULE, {get, #cache_record { k = _K }}, ?CALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Count current items in eCache
%%
%% @spec count() -> integer().
%% @end
%%--------------------------------------------------------------------
count() ->
  gen_server:call(?MODULE, {count}, ?CALL_TIMEOUT).


