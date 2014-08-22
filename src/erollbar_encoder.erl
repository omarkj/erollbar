-module(erollbar_encoder).

-export([create/1,
         encode/3]).

-record(details, {platform :: binary()|undefined,
                  environment :: binary()|undefined,
                  host :: binary()|undefined,
                  branch :: binary()|undefined,
                  send_args :: boolean()
                 }).

-opaque encoder() :: #details{}.
-export_type([encoder/0]).

-spec create(erollbar:opts()) -> encoder().
create(Opts) ->
    #details{platform=proplists:get_value(platform, Opts),
             environment=proplists:get_value(environment, Opts),
             host=proplists:get_value(host, Opts),
             branch=proplists:get_value(branch, Opts),
             send_args=lists:member(send_args, Opts)}.

-spec encode([erollbar_message:msg()], erollbar:access_token(), encoder()) ->
                    [{binary(), term()}].
encode(Items, AccessToken, Details) ->
    [{<<"access_token">>, AccessToken},
     {<<"data">>, [encode_item(Item, Details) || Item <- Items]}].

% Internal
encode_item(Item, Details) ->
    ItemList = erollbar_message:get_proplist(Item),
    ItemEncoded = case erollbar_message:type(Item) of
                      trace ->
                          encode_trace(ItemList, Details);
                      message ->
                          encode_message(ItemList, Details)
                  end,
    %% Add item metadata (things like server, platform, timestamp etc)
    Metadata = encode_metadata(ItemList, Details),
    [{<<"body">>, ItemEncoded}|Metadata].

encode_trace(Trace, #details{send_args=SendArgs}) ->
    {_, Frames} = lists:keyfind(frames, 1, Trace),
    Frames1 = case SendArgs of
                  true -> encode_frames(Frames, keep_args);
                  false -> encode_frames(Frames, scrub_args)
              end,
    {_, Class} = lists:keyfind(class, 1, Trace),
    {_, Message} = lists:keyfind(message, 1, Trace),
    {_, Description} = lists:keyfind(description, 1, Trace),
    Exception = encode_content([{<<"class">>, Class},
                                {<<"message">>, Message},
                                {<<"description">>, Description}]),
    [{<<"trace">>, [{<<"frames">>, Frames1}]},
     {<<"exception">>, Exception}].

encode_message(Message, _Details) ->
    {_, Body} = lists:keyfind(body, 1, Message),
    {_, Metadata} = lists:keyfind(metadata, 1, Message),
    [{<<"message">>, [{<<"body">>, to_binary(Body)}|encode_content(Metadata)]}].

encode_frames(Frames, keep_args) ->
    [encode_frame(Frame) || Frame <- erollbar_message:get_proplist(Frames)];
encode_frames(Frames, scrub_args) ->
    % Frames are to be scrubbed of arguments
    [encode_frame(scrub_frame(Frame)) || Frame <- erollbar_message:get_proplist(Frames)].

encode_frame(Frame) ->
    case lists:keytake(args, 1, Frame) of
        {value, {_, undefined}, Frame1} ->
            encode_content(Frame1);
        {value, {_, Arity}, Frame1} when is_integer(Arity) ->
            case lists:keyfind(method, 1, Frame1) of
                {_, undefined} ->
                    % We have args but no method
                    encode_content(Frame1);
                {_, Method} ->
                    Method1 = iolist_to_binary([to_binary(Method),
                                                <<"/">>, to_binary(Arity)]),
                    % Replace with the new fun/arity method
                    encode_content(lists:keyreplace(method, 1, Frame1, {method, Method1}))
            end;
        {value, {_, Args}, Frame1} ->
            [{<<"args">>, [to_binary(Arg) || Arg <- Args]}|encode_content(Frame1)]
    end.

encode_metadata(ItemList, Details) ->
    add_metadata(timestamp, ItemList, [], Details).

add_metadata(timestamp, ItemList, EncodedItem, Details) ->
    {_, Timestamp} = lists:keyfind(timestamp, 1, ItemList),
    add_metadata(level, ItemList, [{<<"timestamp">>, unix_timestamp(Timestamp)}|EncodedItem], Details);
add_metadata(level, ItemList, EncodedItem, Details) ->
    EncodedItem1 = case lists:keyfind(level, 1, ItemList) of
                       {_, undefined} ->
                           EncodedItem;
                       {_, Level} ->
                           [{<<"level">>, to_binary(Level)}|EncodedItem]
                   end,
    add_metadata(server, ItemList, EncodedItem1, Details);
add_metadata(server, ItemList, EncodedItem, #details{host=Host, branch=Branch}=Details) ->
    Server = encode_content([{<<"host">>, Host},
                             {<<"branch">>, Branch}]),
    add_metadata(platform, ItemList, [{<<"server">>, Server}|EncodedItem], Details);
add_metadata(platform, ItemList, EncodedItem, #details{platform=undefined}=Details) ->
    add_metadata(notifier, ItemList, EncodedItem, Details);
add_metadata(platform, ItemList, EncodedItem, #details{platform=Platform}=Details) ->
    add_metadata(notifier, ItemList, [{<<"platform">>, to_binary(Platform)}|EncodedItem], Details);
add_metadata(notifier, ItemList, EncodedItem, Details) ->
    {ok, Version} = application:get_key(erollbar, vsn),
    add_metadata(language, ItemList, [{<<"notifier">>, [{<<"name">>, <<"erollbar">>},
                                                        {<<"version">>, to_binary(Version)}]}|EncodedItem],
                 Details);
add_metadata(language, ItemList, EncodedItem, Details) ->
    add_metadata(environment, ItemList, [{<<"language">>, <<"erlang">>}|EncodedItem], Details);
add_metadata(environment, _ItemList, EncodedItem, #details{environment=Environment}) ->
    [{<<"environment">>, to_binary(Environment)}|EncodedItem].

encode_content(List) ->
    [{to_binary(Key), to_binary(Val)} || {Key, Val} <- List, Val /= undefined].

scrub_frame(Frame) ->
    case lists:keytake(args, 1, Frame) of
        {value, {_, undefined}, Frame1} ->
            Frame1;
        {value, {_, Args}, Frame1} ->
            case is_integer(Args) of
                true ->
                    [{args, Args}|Frame1];
                false ->
                    [{args, length(Args)}|Frame1]
            end
    end.

unix_timestamp({Mega, Secs, _}) ->
    to_binary(Mega*1000000 + Secs).

-spec to_binary(binary()|list()|integer()|float()|atom()|pid()) ->
                       binary().
to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(A) when is_atom(A) ->
    to_binary(atom_to_list(A));
to_binary(I) when is_integer(I) ->
    to_binary(integer_to_list(I));
to_binary(F) when is_float(F) ->
    to_binary(float_to_list(F));
to_binary(T) when is_tuple(T) ->
    to_binary(io_lib:format("~p", [T]));
to_binary(P) when is_pid(P) ->
    to_binary(pid_to_list(P)).
