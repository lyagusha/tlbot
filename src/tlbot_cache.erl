-module(tlbot_cache).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([get/2,
         get/1,
         set/2]).%{Chat, key}, Val
-define(CHATS_SQL, 
    "SELECT value, chat_id FROM options WHERE key = 'name'").
-define(CONFIG_SQL, 
    "SELECT chat_id, key, value FROM options").
-define(SET_SQL,
    "INSERT INTO options (chat_id, key, value) "
    "VALUES ($1, $2, $3) "
    "ON CONFLICT (chat_id, key) DO  "
    "UPDATE SET value=$3").
get(Key) ->
    get(Key, udefined).

get(Key, Def) ->
    case ets:lookup(?MODULE, Key) of
        [{_, Val}] -> Val;
        [] -> Def
    end.

set(Key, Val) ->
    gen_server:call(?MODULE, {set, Key, Val}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Host} = application:get_env(epgsql, host),
    {ok, User} = application:get_env(epgsql, user),
    {ok, Pass} = application:get_env(epgsql, pass),
    {ok, DB} = application:get_env(epgsql, db),
    {ok, Conn} = epgsql:connect(Host, User, Pass, [{database, DB}]),
    _ = ets:new(?MODULE, [named_table, set, public]),
    {ok, _, Rows} = epgsql:equery(Conn, ?CONFIG_SQL, []),
    _ = [true = ets:insert(?MODULE, {{Chat, Key}, Val})
        ||{Chat, Key, Val} <- Rows],
    {ok, _, Chats} = epgsql:equery(Conn, ?CHATS_SQL, []),
    _ = [true = ets:insert(?MODULE, {{chat, Name}, Id}) 
        ||{Name, Id} <- Chats],
    Env = [begin 
        V = application:get_env(tlbot, Opt, udefined),
        {Opt, V}
        end||Opt <- [token, request_time]],
    true = ets:insert(?MODULE, Env),
    ok = tlbot_events:init(Conn),
    {ok, _} = tlbot_eye:start_link(),
    {ok, [{db_conn, Conn}]}.

handle_call({set, Key, Value}, _, State) ->
    true = ets:insert(?MODULE, {Key, Value}),
    {Chat, SubKey} = Key,
    Conn = proplists:get_value(db_conn, State),
    {ok, _} = epgsql:equery(Conn, ?SET_SQL, [Chat, SubKey, Value]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
