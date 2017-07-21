-module(tlbot_events).
-export([
    init/1,
    make/0,
    search/3,
    morning/1,
    night/1,
    first_day_morning/1,
    last_day_evening/1
]).

-define(EVENTS_SQL,
    "SELECT chat_id, value FROM options WHERE key = 'events'").

%% init
init(DbConn) ->
    _ = ets:new(?MODULE, [bag, public, named_table]),
    {ok, _, Rows} = epgsql:equery(DbConn, ?EVENTS_SQL, []),
    _ = [begin 
        PL = jsx:decode(Json),
        EventsList = proplists:get_value(<<"events">>, PL),
        Objects = [begin
            EventAtom = get_event_atom(Event),
            EventTime = get_event_time(Chat, Event),
            {EventTime, Chat, EventAtom}
        end||Event <- EventsList],
        true = ets:insert(?MODULE, Objects)
    end||{Chat, Json} <- Rows],
    ok.

%% make events
make() ->
    {H, M, _} = time(),
    Events = ets:lookup(?MODULE, {H,M}),
    _ = [?MODULE:Fun(Chat)||{_, Chat, Fun} <- Events].

%% response
search(Chat, Text, Options) ->
    case Text of
        <<"Валюта"/utf8>> -> currency(Chat);
        <<"валюта"/utf8>> -> currency(Chat);
        Q -> parse_complex_query(Chat, Q, Options)
    end.

parse_complex_query(Chat, Query, Options) ->
    case re:run(Query, "currency_") of                                            
         {match, _} ->                                                            
             [_, Curr] = re:split(Query, "currency_", [{return, list}]),           
             currency_resp(Chat, Curr, Options);                                                  
         _ -> ignore                                                              
    end,
    ok.

%% events
morning(Chat) ->
    Text = get_msg(Chat, morning),
    tlbot_requester:msg(Chat, Text).

night(Chat) ->
    Text = get_msg(Chat, night),
    tlbot_requester:msg(Chat, Text).

% TODO day of week
first_day_morning(Chat) ->
    Text = get_msg(Chat, first_day_morning),
    tlbot_requester:msg(Chat, Text).

% TODO day of week
last_day_evening(Chat) ->
    Text = get_msg(Chat, last_day_evening),
    tlbot_requester:msg(Chat, Text).

currency(Chat) ->
    CurrJson = case tlbot_cache:get({Chat, currency}, 0) of
        0 -> tlbot_cache:get({0, <<"currency">>});
        J -> J
    end,
    CurrList = proplists:get_value(<<"currency">>, jsx:decode(CurrJson)),
    Buttons = [[{<<"text">>, X},                                                 
                {<<"callback_data">>, <<"currency_", X/binary>>}] 
               || X <- CurrList],
    Rows = rows(Buttons),
    Text = <<"Выберите интересующую вас валюту из списка ниже..."/utf8>>,
    Options = [{reply_markup, [{<<"inline_keyboard">>, Rows}]}],
    MsgId = tlbot_requester:msg(Chat, Text, Options),
    _ = timer:apply_after(120000, tlbot_requester, del_msg, [Chat, MsgId]),
    ok.
% TODO !!!!!!!!!
currency_resp(Chat, Curr, Options) ->
    case tlbot_requester:nbu(Curr) of
        [[]] -> ignore;
        Resp ->
            [Decoded] = jsx:decode(list_to_binary(Resp)),
            Rate = proplists:get_value(<<"rate">>, Decoded),
            RateBin = float_to_binary(Rate, [{decimals, 6}]),
            Txt = proplists:get_value(<<"txt">>, Decoded),
            Date = proplists:get_value(<<"exchangedate">>, Decoded), 
            User = proplists:get_value(<<"from">>, Options),
            FName = proplists:get_value(<<"first_name">>, User),
            RespMsg = <<FName/binary,", курс "/utf8, Txt/binary, 
                " по состоянию на "/utf8, Date/binary, 
                " равен "/utf8, RateBin/binary>>,
            Id = tlbot_requester:msg(Chat, RespMsg),
            _ = timer:apply_after(120000, tlbot_requester, del_msg, [Chat, Id])  
    end.

%% internal
get_msg(Chat, Type) when is_binary(Chat) ->
    ChatId = tlbot_cache:get({chat, Chat}),
    get_msg(ChatId, Type);
get_msg(Chat, Type) when is_integer(Chat)-> 
    TypeBin = atom_to_binary(Type, utf8),
    MsgsJson=case tlbot_cache:get({Chat,Key = <<TypeBin/binary,"_msgs">>}, 0) of
        0 -> tlbot_cache:get({0, Key});
        Json -> Json
    end,
    MsgsPL = jsx:decode(MsgsJson),
    Msgs = proplists:get_value(Key, MsgsPL),
    Text = lists:nth(rand:uniform(length(Msgs)), Msgs),
    Text.

get_event_atom(Event) ->
    case Event of
        <<"morning">> -> morning;
        <<"night">> -> night;
        <<"last_day_evening">> -> last_day_evening;
        <<"first_day_morning">> -> first_day_morning
    end.

get_event_time(Chat, Event) ->
    Key = <<Event/binary, "_time">>,  
    TimeText = case tlbot_cache:get({Chat, Key}, 0) of
        0 -> tlbot_cache:get({0, Key});
        Val -> Val
    end,
    parse_time_text(TimeText, <<>>).

parse_time_text(<<S, Rest/binary>>, H) ->
    case [S] of 
        ":" -> 
            {binary_to_integer(H), binary_to_integer(Rest)};
        _ ->
            parse_time_text(Rest, <<H/binary, S>>)
    end.

rows(X) ->                                                                       
    rows(X, []).                                                                 
                                                                                  
rows([A,B,C,D,E,F|Rest], Res) ->                                                 
    rows(Rest, [[A,B,C,D,E,F]|Res]);                                             
rows([], Res) ->                                                                 
    lists:reverse(Res);                                                          
rows(Rest, Res) ->                                                               
    lists:reverse([Rest|Res]).





















