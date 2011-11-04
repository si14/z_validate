-module(z_validate_example).
-export([start/0]).

-include("z_validate.hrl").

start() ->
    %% well-behaved input
    GoodUserInput = [{foo, <<"3">>},
                     {bar, <<"foobar">>}],
    %% not so well-behaved inputs
    BadUserInput1 = [{foo, <<"foo">>}],
    BadUserInput2 = [{foo, <<"3">>},
                     {bar, <<"rm -rf /usr">>}],
    [handle_output(handle_input(Input))
     || Input <- [GoodUserInput, BadUserInput1, BadUserInput2]],
    ok.

handle_output({ok, Result}) ->
    io:format("It's OK, result: ~p~n", [Result]);
handle_output({error, Error}) ->
    io:format("Error happens: ~p~n", [err_to_str(Error)]).

err_to_str(error_in_foo) -> "you have an error in your foo!";
err_to_str(error_in_bar) -> "you have an error in your bar!";
err_to_str(bad_guy)      -> "looks like you are bad guy!".

handle_input(Input) ->
    try
        MaybeInput = z_wrap(Input, input_error),
        Foo = z_bin_to_int(z_proplist_get(MaybeInput, {foo}, error_in_foo)),
        SmallFoo = z_int_in_range(Foo, {1, 10}),
        BarBin = z_proplist_get(MaybeInput, {bar}, error_in_bar),
        MaybeBadBar = z_bin_to_list(BarBin),
        GoodBar1 = z_verify(fun justletters/1, MaybeBadBar, bad_guy),
        GoodBar2 = z_only_loweralpha(MaybeBadBar, bad_guy),
        GoodBar2 = GoodBar1,
        GoodBarUpper = z_apply(fun upper/1, GoodBar1),
        z_return({z_unwrap(SmallFoo),
                  z_unwrap(GoodBarUpper)})
    catch
        ?Z_OK(Result)   -> {ok, Result};
        ?Z_ERROR(Error) -> {error, Error}
    end.

justletters(Var) ->
    lists:all(fun(C) -> C >= $a andalso C =< $z end, Var).

upper(Var) ->
    {ok, string:to_upper(Var)}.
