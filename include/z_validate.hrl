-define(Z_OK(VAR), throw:{z_validate, {ok, VAR}}).
-define(Z_ERROR(VAR), throw:{z_validate, {error, VAR}}).
-import(z_validate, [z_wrap/2, z_unwrap/1,
                     z_proplist_get/2, z_proplist_get/3,
                     z_int_in_range/2, z_int_in_range/3,
                     z_bin_to_int/1, z_bin_to_int/2,
                     z_bin_to_list/1, z_bin_to_list/2,
                     z_only_loweralpha/1, z_only_loweralpha/2,
                     z_verify/2, z_verify/3,
                     z_apply/2, z_apply/3,
                     z_return/1]).

%% z_wrap(Input, Error) ->
%%     z_validate:wrap(Input, Error).

%% z_proplist_get(Input, Args) ->
%%     z_validate:proplist_get(Input, Args).
%% z_proplist_get(Input, Args, Error) ->
%%     z_validate:proplist_get(Input, Args, Error).

%% z_int_in_range(Input, Args) ->
%%     z_validate:int_in_range(Input, Args).
%% z_int_in_range(Input, Args, Error) ->
%%     z_validate:int_in_range(Input, Args, Error).

%% z_bin_to_int(Input) ->
%%     z_validate:bin_to_int(Input).
%% z_bin_to_int(Input, Error) ->
%%     z_validate:bin_to_int(Input, Error).

%% z_bin_to_list(Input) ->
%%     z_validate:bin_to_list(Input).
%% z_bin_to_list(Input, Error) ->
%%     z_validate:bin_to_list(Input, Error).

%% z_only_loweralpha(Input) ->
%%     z_validate:z_only_loweralpha(Input).
%% z_only_loweralpha(Input, Error) ->
%%     z_validate:z_only_loweralpha(Input, Error).

%% z_verify(Fun, Input) ->
%%     z_validate:verify(Fun, Input).
%% z_verify(Fun, Input, Error) ->
%%     z_validate:verify(Fun, Input, Error).

%% z_apply(Fun, Input) ->
%%     z_validate:z_apply(Fun, Input).
%% z_apply(Fun, Input, Error) ->
%%     z_validate:z_apply(Fun, Input, Error).

%% z_return(Input) ->
%%     z_validate:return(Input).
