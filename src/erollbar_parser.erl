-module(erollbar_parser).

-export([prime/1
        ,initial_call/1
        ,parse/2
        ,create_message/3
        ]).

-record(details, {platform :: binary()|undefined,
                  environment :: binary()|undefined,
                  host :: binary()|undefined,
                  branch :: binary()|undefined,
                  sha :: binary()|undefined,
                  send_args :: boolean()
                 }).

prime(Opts) ->
    #details{platform=proplists:get_value(platform, Opts),
             environment=proplists:get_value(environment, Opts),
             host=proplists:get_value(host, Opts),
             branch=proplists:get_value(branch, Opts),
             sha=proplists:get_value(sha, Opts),
             send_args=lists:member(send_args, Opts)}.

initial_call({_, _, {_, _, [Report, _]}}) ->
    {Module, _, _} = proplists:get_value(initial_call, Report),
    Module.


parse({_, _, {_, _, [Report, _]}}, #details{platform=Platform}=Details) ->
    ErrorInfo = proplists:get_value(error_info, Report, []),
    Item = [{<<"trace">>, create_trace(ErrorInfo, Details)},
            {<<"level">>, <<"error">>},
            {<<"timestamp">>, unix_timestamp()},
            {<<"platform">>, Platform},
            {<<"language">>, <<"erlang">>}],
    {ok, Item}.

create_message(AccessToken, Items, Details) ->
    ItemBlocks = create_items_block(Items, [], Details),
    Message = [{<<"access_token">>, AccessToken},
               {<<"data">>, ItemBlocks}],
    jsx:encode(Message).

create_items_block([], Retval, _) ->
    Retval;
create_items_block([Item|Items], Retval, #details{environment=Environment,
                                                  host=Host,
                                                  branch=Branch,
                                                  sha=Sha}=Details) ->
    ItemBlock = [{<<"environment">>, Environment},
                 {<<"body">>, Item}],
    ItemBlock1 =
        case create_server_block(Host, Branch, Sha) of
            [] ->
                ItemBlock;
            ServerBlock ->
                [{<<"server">>, ServerBlock} | ItemBlock]
        end,
    create_items_block(Items, [ItemBlock1 | Retval], Details).

create_server_block(Host, Branch, Sha) ->
    Block = add_to_block(<<"host">>, Host, []),
    Block1 = add_to_block(<<"branch">>, Branch, Block),
    add_to_block(<<"sha">>, Sha, Block1).

add_to_block(_, undefined, Block) ->
    Block;
add_to_block(Key, Val, Block) ->
    [{Key, Val} | Block].

% Internal
create_trace({_Error, ExceptionExit, Frames}, Details) when is_atom(ExceptionExit) ->
    [{<<"frames">>, create_frames(Frames, Details, [])},
     {<<"exception">>, [{<<"class">>, to_binary(ExceptionExit)}]}];
create_trace({_Error, {ExceptionExit, Frames}, _}, Details) ->
    [{<<"frames">>, create_frames(Frames, Details, [])},
     {<<"exception">>, [{<<"class">>, to_binary(ExceptionExit)}]}].

create_frames([], _, Retval) ->
    Retval;
create_frames([{Module, Fun, ArgsOrArity, Info}|Rest], #details{send_args=SendArgs}=Details,
              Retval) ->
    Filename = iolist_to_binary([to_binary(Module), ".erl"]),
    Frame = [{<<"filename">>, to_binary(Filename)}],
    Frame1 = add_method_args(Fun, ArgsOrArity, Frame, SendArgs),
    Frame2 = add_lineno(proplists:get_value(line, Info), Frame1),
    create_frames(Rest, Details, Retval ++ [Frame2]).

add_method_args(Fun, Arity, Frame, _) when is_integer(Arity) ->
    Frame ++ [{<<"method">>, iolist_to_binary([to_binary(Fun),
                                               <<"/">>, to_binary(Arity)])}];
add_method_args(Fun, Args, Frame, SendArgs) when is_list(Args) ->
    if SendArgs ->
            Frame ++ [{<<"method">>, to_binary(Fun)},
                      {<<"args">>, [to_binary(Arg) || Arg <- Args]}];
       true ->
            add_method_args(Fun, length(Args), Frame, SendArgs)
    end.

add_lineno(LineNo, Frame) when is_integer(LineNo) ->
    Frame ++ [{<<"lineno">>, LineNo}];
add_lineno(_, Frame) ->
    Frame.

unix_timestamp() ->
    {Mega, Secs, _} = now(),
    to_binary(Mega*1000000 + Secs).

-spec to_binary(binary()|list()|integer()|float()|atom()) ->
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
    to_binary(io_lib:format("~p", [T])).

