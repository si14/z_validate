-module(z_validate).
-export([z_wrap/1, z_wrap/2, z_unwrap/1,
         z_in_list/2, z_in_list/3,
         z_proplist_get/2, z_proplist_get/3,
         z_int_in_range/2, z_int_in_range/3,
         z_bin_to_int/1, z_bin_to_int/2,
         z_bin_to_pos_int/1, z_bin_to_pos_int/2,
         z_bin_to_list/1, z_bin_to_list/2,
         z_bin_to_bool/1, z_bin_to_bool/2,
         z_bin_to_ex_atom/1, z_bin_to_ex_atom/2,
         z_only_loweralpha_str/1, z_only_loweralpha_str/2,
         z_is_email_str/1, z_is_email_str/2,
         z_is_email_bin/1, z_is_email_bin/2,
         z_verify/2, z_verify/3,
         z_apply/2, z_apply/3,
         z_return/1]).

-define(THROW_Z_ERROR(ERROR), throw({z_throw, {error, ERROR}})).
-define(THROW_Z_OK(RESULT),   throw({z_throw, {ok, RESULT}})).

-define(Z_VALUE(VAL, ERR), {z_value, VAL, ERR}).

-define(Z_TRANSFORM(TRANSFORMER, ERROR),
        case catch TRANSFORMER of
            X -> ?Z_VALUE(X, ERROR);
            _ -> ?THROW_Z_ERROR(ERROR)
        end).
-define(Z_TRANSFORM(TRANSFORMER, GUARD, ERR),
        case catch TRANSFORMER of
            X when GUARD(X) -> ?Z_VALUE(X, ERR);
            _               -> ?THROW_Z_ERROR(ERR)
        end).
-define(Z_CHECK_OLD_ZVAL(CHECKER, ZVAL, ERR), % this one don't reconstruct
        case catch CHECKER of
            true -> ZVAL;
            _    -> ?THROW_Z_ERROR(ERR)
        end).
-define(Z_CHECK_NEW_ZVAL(CHECKER, VAL, ERR),
        case catch CHECKER of
            true -> ?Z_VALUE(VAL, ERR);
            _    -> ?THROW_Z_ERROR(ERR)
        end).

-type error_term() :: any().
-type z_value(T)   :: {z_value, T, error_term()}.

%%% Primitives.

-spec z_wrap(T | z_value(T)) -> z_value(T).
z_wrap(X) -> z_wrap(X, undefined).

-spec z_wrap(T | z_value(T), error_term()) -> z_value(T).
z_wrap(?Z_VALUE(Val, _OldErr), Err) -> ?Z_VALUE(Val, Err);
z_wrap(Val, Err) -> ?Z_VALUE(Val, Err).

-spec z_unwrap(z_value(T)) -> T.
z_unwrap(?Z_VALUE(Val, _Error)) -> Val.

-spec z_return(any()) -> none().
z_return(Val) -> ?THROW_Z_OK(Val).

%% This function doesn't use z_verify/3 to not reconstruct Z_VALUE.
-spec z_verify(fun((A) -> boolean()), z_value(A)) -> z_value(A).
z_verify(Fun, ?Z_VALUE(Val, Err)=ZVal) ->
    ?Z_CHECK_OLD_ZVAL(Fun(Val), ZVal, Err).

-spec z_verify(fun((A) -> boolean()),
               z_value(A),
               error_term()) -> z_value(A).
z_verify(Fun, ?Z_VALUE(Val, _OldErr), NewErr) ->
    ?Z_CHECK_NEW_ZVAL(Fun(Val), Val, NewErr).

-spec z_apply(fun((A) -> {ok, B} | any()),
              z_value(A)) -> z_value(B).
z_apply(Fun, ?Z_VALUE(_, Err)=ZVal) ->
    z_apply(Fun, ZVal, Err).

-spec z_apply(fun((A) -> {ok, B} | any()),
              z_value(A), error_term()) -> z_value(B).
z_apply(Fun, ?Z_VALUE(Val, _Err), NewErr) ->
    case catch Fun(Val) of
        {ok, NewVal} -> ?Z_VALUE(NewVal, NewErr);
        _            -> ?THROW_Z_ERROR(NewErr)
    end.

%%% Transformers

-spec z_bin_to_ex_atom(z_value(binary())) -> z_value(atom()).
z_bin_to_ex_atom(?Z_VALUE(_Val, Err)=ZVal) ->
    z_bin_to_ex_atom(ZVal, Err).

-spec z_bin_to_ex_atom(z_value(binary()), error_term()) -> z_value(atom()).
z_bin_to_ex_atom(?Z_VALUE(Val, _Err), NewErr) ->
    ?Z_TRANSFORM(binary_to_existing_atom(Val, utf8), is_atom, NewErr).

-spec z_bin_to_int(z_value(binary())) -> z_value(integer()).
z_bin_to_int(?Z_VALUE(_Val, Err)=ZVal) ->
    z_bin_to_int(ZVal, Err).

-spec z_bin_to_int(z_value(binary()), error_term()) -> z_value(integer()).
z_bin_to_int(?Z_VALUE(Val, _Err), NewErr) ->
    ?Z_TRANSFORM(list_to_integer(binary_to_list(Val)), is_integer, NewErr).

-spec z_bin_to_pos_int(z_value(binary())) -> z_value(pos_integer()).
z_bin_to_pos_int(?Z_VALUE(_Val, Err)=ZVal) ->
    z_bin_to_pos_int(ZVal, Err).

-spec z_bin_to_pos_int(z_value(binary()), error_term()) -> z_value(pos_integer()).
z_bin_to_pos_int(ZVal, NewErr) ->
    z_verify(fun (X) -> X >= 0 end, z_bin_to_int(ZVal, NewErr)).

-spec z_bin_to_list(z_value(binary())) -> z_value(list()).
z_bin_to_list(?Z_VALUE(_Val, Err)=ZVal) ->
    z_bin_to_list(ZVal, Err).

-spec z_bin_to_list(z_value(binary()), error_term()) -> z_value(list()).
z_bin_to_list(?Z_VALUE(Val, _Err), NewErr) ->
    ?Z_TRANSFORM(binary_to_list(Val), is_list, NewErr).

-spec z_bin_to_bool(z_value(binary())) -> z_value(boolean()).
z_bin_to_bool(?Z_VALUE(_Val, Err)=ZVal) ->
    z_bin_to_bool(ZVal, Err).

-spec z_bin_to_bool(z_value(binary()), error_term()) -> z_value(boolean()).
z_bin_to_bool(?Z_VALUE(Val, _Err), NewErr) ->
    ?Z_TRANSFORM(binary_to_existing_atom(Val, utf8), is_boolean, NewErr).


%%% Collections-related stuff

-spec z_proplist_get(z_value([{A, B}]), {A}) -> z_value(B).
z_proplist_get(?Z_VALUE(_Val, Err)=ZVal, Args) ->
    z_proplist_get(ZVal, Args, Err).

-spec z_proplist_get(z_value([{A, B}]), {A}, error_term()) -> z_value(B).
z_proplist_get(?Z_VALUE(Val, _Err), {Key}, NewErr) ->
    case catch lists:keyfind(Key, 1, Val) of
        {Key, NewVal} -> ?Z_VALUE(NewVal, NewErr);
        _             -> ?THROW_Z_ERROR(NewErr)
    end.

%%% Checkers

-spec z_int_in_range(z_value(integer()),
                     {integer() | any, integer() | any}) -> z_value(integer()).
z_int_in_range(?Z_VALUE(Val, Err)=ZVal, {Low, High}) ->
    ?Z_CHECK_OLD_ZVAL(is_integer(Val)
                      andalso (Low =:= any orelse Val >= Low)
                      andalso (High =:= any orelse Val =< High),
                      ZVal, Err).

-spec z_int_in_range(z_value(integer()),
                     {integer() | any, integer() | any},
                     error_term()) -> z_value(integer()).
z_int_in_range(?Z_VALUE(Val, _Err), {Low, High}, NewErr) ->
    ?Z_CHECK_NEW_ZVAL(is_integer(Val)
                      andalso (Low =:= any orelse Val >= Low)
                      andalso (High =:= any orelse Val =< High),
                      Val, NewErr).

-spec z_in_list(z_value(T), {list(T)}) -> z_value(T).
z_in_list(?Z_VALUE(Val, Err)=ZVal, {Lst}) ->
    ?Z_CHECK_OLD_ZVAL(lists:member(Val, Lst), ZVal, Err).

-spec z_in_list(z_value(T), {list(T)}, error_term()) -> z_value(T).
z_in_list(?Z_VALUE(Val, _Err), {Lst}, NewErr) ->
    ?Z_CHECK_NEW_ZVAL(lists:member(Val, Lst), Val, NewErr).

-spec z_only_loweralpha_str(z_value(string())) -> z_value(string()).
z_only_loweralpha_str(?Z_VALUE(Val, Err)=ZVal) ->
    ?Z_CHECK_OLD_ZVAL(test_only_loweralpha_str(Val), ZVal, Err).

-spec z_only_loweralpha_str(z_value(string()),
                            error_term()) -> z_value(string()).
z_only_loweralpha_str(?Z_VALUE(Val, _Err), NewErr) ->
    ?Z_CHECK_NEW_ZVAL(test_only_loweralpha_str(Val), Val, NewErr).

-spec z_is_email_str(z_value(string())) -> z_value(string()).
z_is_email_str(?Z_VALUE(Val, Err)=ZVal) ->
    ?Z_CHECK_OLD_ZVAL(test_email_str(Val), ZVal, Err).

-spec z_is_email_str(z_value(string()),
                     error_term()) -> z_value(string()).
z_is_email_str(?Z_VALUE(Val, _Err), NewErr) ->
    ?Z_CHECK_NEW_ZVAL(test_email_str(Val), Val, NewErr).

-spec z_is_email_bin(z_value(binary())) -> z_value(binary()).
z_is_email_bin(?Z_VALUE(Val, Err)=ZVal) ->
    ?Z_CHECK_OLD_ZVAL(test_email_bin(Val), ZVal, Err).

-spec z_is_email_bin(z_value(binary()),
                     error_term()) -> z_value(binary()).
z_is_email_bin(?Z_VALUE(Val, _Err), NewErr) ->
    ?Z_CHECK_NEW_ZVAL(test_email_bin(Val), Val, NewErr).


%%% Internal

test_only_loweralpha_str([]) -> true;
test_only_loweralpha_str([C|Cs]) when (is_integer(C)
                                       andalso C >= $a
                                       andalso C =< $z) ->
    test_only_loweralpha_str(Cs);
test_only_loweralpha_str([_]) -> false.

test_email_bin(Bin) ->
    test_email_str(binary_to_list(Bin)).

test_email_str(Str) ->
    case re:run(Str, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]+") of
        {match, _} -> true;
        _          -> false
    end.
