-module(tlbot_eye).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PGET(Key, List, Def), proplists:get_value(Key, List, Def)).

cast(Req) ->
    gen_server:cast(?MODULE, Req).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Timer} = timer:send_after(50, upd), 
    {ok, Timer1} = timer:send_after(50, event),
    {ok, [{upd_timer, Timer}, {event_timer, Timer1}]}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({parse, Upds}, State) ->
    _ = parse_upds(Upds),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(upd, State) ->
    Timer = proplists:get_value(upd_timer, State),
    {ok, cancel} = timer:cancel(Timer),
    Upds = tlbot_requester:upd(),
    case Upds of 
        [] -> ignore;
        _ -> ok = cast({parse, Upds})
    end,
    {ok, NewTimer} = timer:send_after(500, upd),
    NewState = lists:keyreplace(upd_timer, 2, State, {upd_timer, NewTimer}),
    {noreply, NewState};
handle_info(event, State) ->
    {ok, NewTimer} = timer:send_after(60000, event),
    tlbot_events:make(),
    Timer = proplists:get_value(event_timer, State),
    {ok, cancel} = timer:cancel(Timer),
    NewState = lists:keyreplace(event_timer, 2, State, {event_timer, NewTimer}),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
parse_upds([Upd|Upds]) ->
    Msg = ?PGET(<<"message">>, Upd, 0),
    InlineQuery = ?PGET(<<"callback_query">>, Upd, 0),
    if
        Msg /= 0 -> parse_msg(Msg);
        InlineQuery /= 0 -> parse_inline_query(InlineQuery)
    end,
    parse_upds(Upds);
parse_upds([]) ->
    ok.

%TODO match
parse_msg(Msg) ->
    Chat = proplists:get_value(<<"chat">>, Msg),
    ChatId = proplists:get_value(<<"id">>, Chat),
    Text = proplists:get_value(<<"text">>, Msg),
    User = proplists:get_value(<<"from">>, Msg),
    FName = proplists:get_value(<<"first_name">>, User),
    LName = proplists:get_value(<<"last_name">>, User, <<>>),
    ChatName = chat_name(ChatId),
    io:format("~nNEW MSG FROM ~ts ~ts in chat ~ts:~n\"~ts\"",
        [FName, LName, ChatName, Text]),
    tlbot_events:search(ChatId, Text, []),
    ok.
parse_inline_query(Query) ->
    User = proplists:get_value(<<"from">>, Query),
    FName = proplists:get_value(<<"first_name">>, User),
    LName = proplists:get_value(<<"last_name">>, User, <<>>),
    Msg = proplists:get_value(<<"message">>, Query),
    Chat = proplists:get_value(<<"chat">>, Msg),
    ChatId = proplists:get_value(<<"id">>, Chat),
    ChatName = chat_name(ChatId),
    Text = proplists:get_value(<<"data">>, Query),
    io:format("~nNEW QUERY FROM ~ts ~ts in chat ~ts:~n\"~ts\"",
        [FName, LName, ChatName, Text]),
    tlbot_events:search(ChatId, Text, [{<<"from">>, User}]),
    ok.

chat_name(ChatId) ->
    case tlbot_cache:get({ChatId, <<"name">>}, 0) of
        0 ->
            IdBin = integer_to_binary(ChatId),
            NewName = <<"unknown chat id: ", IdBin/binary>>,
            tlbot_cache:set({ChatId, <<"name">>}, NewName),
            NewName;
        Name -> Name
    end.





