-module(z_validate).
-export([z_wrap/1, z_wrap/2, z_unwrap/1,
         z_in_list/2, z_in_list/3,
         z_proplist_get/2, z_proplist_get/3,
         z_int_in_range/2, z_int_in_range/3,
         z_bin_to_int/1, z_bin_to_int/2,
         z_bin_to_list/1, z_bin_to_list/2,
         z_bin_to_bool/1, z_bin_to_bool/2,
         z_bin_to_ex_atom/1, z_bin_to_ex_atom/2,
         z_only_loweralpha/1, z_only_loweralpha/2,
         z_verify/2, z_verify/3,
         z_apply/2, z_apply/3,
         z_return/1]).

-define(THROW_Z_ERROR(ERROR), throw({z_validate, {error, ERROR}})).
-define(THROW_Z_OK(RESULT),   throw({z_validate, {ok, RESULT}})).

-type error_atom() :: atom().
-type z_value(T) :: {T, error_atom()}.


%%% Primitives.

-spec z_wrap(T, error_atom()) -> z_value(T).
z_wrap(Val, Error) -> {Val, Error}.

-spec z_wrap(T) -> z_value(T).
z_wrap(Val) -> {Val, undefined}.

-spec z_unwrap(z_value(T)) -> T.
z_unwrap({Val, _Error}) -> Val.

-spec z_return(any()) -> none().
z_return(Val) -> ?THROW_Z_OK(Val).

-spec z_verify(fun((A) -> boolean()), z_value(A)) -> z_value(A).
z_verify(Fun, {Val, Error}=Input) ->
    case catch Fun(Val) of
        true -> Input;
        _    -> ?THROW_Z_ERROR(Error)
    end.

-spec z_verify(fun((A) -> boolean()),
               z_value(A),
               error_atom()) -> z_value(A).
z_verify(Fun, {Val, _Error}, NewError) ->
    case catch Fun(Val) of
        true -> {Val, NewError};
        _    -> ?THROW_Z_ERROR(NewError)
    end.

-spec z_apply(fun((A) -> B), z_value(A)) -> z_value(B).
z_apply(Fun, {Val, Error}) ->
    case catch Fun(Val) of
        {ok, X} -> {X, Error};
        _       -> ?THROW_Z_ERROR(Error)
    end.

-spec z_apply(fun((A) -> B), z_value(A), error_atom()) -> z_value(B).
z_apply(Fun, {Val, _Error}, NewError) ->
    case catch Fun(Val) of
        {ok, X} -> {X, NewError};
        _       -> ?THROW_Z_ERROR(NewError)
    end.


%%% Wrapped library functions.

-spec z_proplist_get(z_value([{A, B}]), {A}) -> z_value(B).
z_proplist_get({_Val, Error} = Z, Args) -> z_proplist_get(Z, Args, Error).

-spec z_proplist_get(z_value([{A, B}]), {A}, error_atom()) -> z_value(B).
z_proplist_get({Val, _Error}, {Key}, NewError) ->
    case catch lists:keyfind(Key, 1, Val) of
        {Key, NewVal} -> {NewVal, NewError};
        _             -> ?THROW_Z_ERROR(NewError)
    end.

-spec z_int_in_range(z_value(integer()),
                     {integer() | any, integer() | any}) -> z_value(integer()).
z_int_in_range({_Val, Error}=Z, Args) -> z_int_in_range(Z, Args, Error).

-spec z_int_in_range(z_value(integer()),
                     {integer() | any, integer() | any},
                     error_atom()) -> z_value(integer()).
z_int_in_range({Val, _Error}, {Low, High}, NewError) ->
    case (is_integer(Val)
          andalso (Low =:= any orelse Val >= Low)
          andalso (High =:= any orelse Val =< High)) of
        true  -> {Val, NewError};
        false -> ?THROW_Z_ERROR(NewError)
    end.

-spec z_bin_to_ex_atom(z_value(binary())) -> z_value(atom()).
z_bin_to_ex_atom({_Val, Error}=Z) -> z_bin_to_ex_atom(Z, Error).

-spec z_bin_to_ex_atom(z_value(binary()), error_atom()) -> z_value(atom()).
z_bin_to_ex_atom({Val, _Error}, NewError) ->
    case catch list_to_existing_atom(binary_to_list(Val)) of
        X when is_atom(X) -> {X, NewError};
        _    -> ?THROW_Z_ERROR(NewError)
    end.

-spec z_bin_to_int(z_value(binary())) -> z_value(integer()).
z_bin_to_int({_Val, Error}=Z) -> z_bin_to_int(Z, Error).

-spec z_bin_to_int(z_value(binary()), error_atom()) -> z_value(integer()).
z_bin_to_int({Val, _Error}, NewError) ->
    case catch list_to_integer(binary_to_list(Val)) of
        X when is_integer(X) -> {X, NewError};
        _                    -> ?THROW_Z_ERROR(NewError)
    end.

-spec z_bin_to_list(z_value(binary())) -> z_value(list()).
z_bin_to_list({_Val, Error}=Z) -> z_bin_to_list(Z, Error).

-spec z_bin_to_list(z_value(binary()), error_atom()) -> z_value(list()).
z_bin_to_list({Val, _Error}, NewError) ->
    case catch binary_to_list(Val) of
        X when is_list(X) -> {X, NewError};
        _                 -> ?THROW_Z_ERROR(NewError)
    end.

-spec z_bin_to_bool(z_value(binary())) -> z_value(boolean()).
z_bin_to_bool({_Val, Error} = ZVal) -> z_bin_to_bool(ZVal, Error).

-spec z_bin_to_bool(z_value(binary()), error_atom()) -> z_value(boolean()).
z_bin_to_bool({Val, _Error}, NewError) ->
    case catch list_to_existing_atom(binary_to_list(Val)) of
        X when is_boolean(X) -> {X, NewError};
        _                    -> ?THROW_Z_ERROR(NewError)
    end.

-spec z_in_list(z_value(T), {list(T)}) -> z_value(T).
z_in_list({_Val, Error}=Z, Args) -> z_in_list(Z, Args, Error).

-spec z_in_list(z_value(T), {list(T)}, error_atom()) -> z_value(T).
z_in_list({Val, _Error}, {Lst}, NewError) ->
    case catch lists:member(Val, Lst) of
        true -> {Val, NewError};
        _    -> ?THROW_Z_ERROR(NewError)
    end.

-spec z_only_loweralpha(z_value(string())) -> z_value(string()).
z_only_loweralpha({_Val, Error}=Z) -> z_only_loweralpha(Z, Error).

-spec z_only_loweralpha(z_value(string()), error_atom()) -> z_value(string()).
z_only_loweralpha({Val, _Error}, NewError) ->
    case catch test_only_loweralpha(Val) of
        true -> {Val, NewError};
        _    -> ?THROW_Z_ERROR(NewError)
    end.

test_only_loweralpha([]) -> true;
test_only_loweralpha([C|Cs]) ->
    case (is_integer(C)
          andalso C >= $a
          andalso C =< $z) of
        true -> test_only_loweralpha(Cs);
        false -> false
    end.
