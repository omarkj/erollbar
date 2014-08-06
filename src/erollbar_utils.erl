-module(erollbar_utils).

-export([request/3,
         to_binary/1]).

-spec request(binary(), binary(), integer()) ->
                     ok|{error, pos_integer(), binary()}|
                     {error, term()}.
request(Endpoint, Message, Timeout) ->
    Resource = <<Endpoint/binary, "/items/">>,
    case hackney:request(post, Resource, [{<<"content-type">>, <<"application/json">>}],
                         Message, [{timeout, Timeout}]) of
        {ok, 200, _Headers, ClientRef} ->
            hackney:close(ClientRef),
            ok;
        {ok, Code, _Header, ClientRef} ->
            Body = get_body(ClientRef),
            hackney:close(ClientRef),
            {error, Code, Body};
        {error, _} = E ->
            E
    end.

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
    to_binary(float_to_list(F)).

%% Internal
get_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {ok, Body} ->
            Body;
        _ ->
            <<>>
    end.
