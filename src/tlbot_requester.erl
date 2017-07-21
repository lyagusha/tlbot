-module(tlbot_requester).
-export([make/2,
         upd/0,
         msg/3,
         msg/2,
         del_msg/2,
         nbu/1
    ]).

make(Action, Body) ->
    Token = tlbot_cache:get(token),
    Url = "https://api.telegram.org/bot"++Token++"/"++Action,
    Type = "application/json",
    Resp = case httpc:request(post, {Url, [], Type, Body}, [], []) of
        {ok, {{_, _Code, _Msg}, _Headers, R}} -> R;
        Err -> Err
    end,
    try jsx:decode(list_to_binary(Resp)) of                
        L when is_list(L) -> L                                
    catch
        _:_ ->
            true = ets:insert(tlbot_cache, {bad_resp, {?MODULE,?LINE}, Resp}),
            []
    end.

nbu(Curr) when is_binary(Curr)->
    nbu(binary_to_list(Curr));
nbu(Curr) when is_list(Curr) ->
    Res = httpc:request("http://bank.gov.ua/NBUStatService/v1/statdirectory/exchange?valcode="++ Curr ++"&json"),
    case Res of
        {ok, {_, _, Resp}} -> Resp;
        Err -> Err
    end.

del_msg(Chat, Msg) ->
    make("deletemessage", 
        jsx:encode([{<<"chat_id">>, Chat}, {<<"message_id">>, Msg}])).

%TODO parse response
msg(Chat, Text) ->
    msg(Chat, Text, []).

msg(Chat, Text, Options) when is_integer(Chat) ->
    Resp = make("sendmessage", 
        jsx:encode([{chat_id, Chat}, {text, Text}|Options])),
    Result = proplists:get_value(<<"result">>, Resp, []),
    proplists:get_value(<<"message_id">>, Result);
msg(Chat, Text, Options) when is_binary(Chat) ->
    ChatId = tlbot_cache:get({chat, Chat}),
    msg(ChatId, Text, Options).


upd() ->
    UpdId = tlbot_cache:get(last_upd, 0),
    Body = jsx:encode([{<<"offset">>, UpdId+1}, {<<"allowed_updates">>, []}]),
    Resp = make("getupdates", Body),
    Upds = proplists:get_value(<<"result">>, Resp, []),
    case Upds of
        [] -> ok;
        _ ->
            LastUpd = lists:last(Upds),
            LastUpdId = proplists:get_value(<<"update_id">>, LastUpd),
            true = ets:insert(tlbot_cache, {last_upd, LastUpdId})
    end,
    Upds.
    

