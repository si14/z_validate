-define(Z_OK(VAR), throw:{z_throw, {ok, VAR}}).
-define(Z_ERROR(VAR), throw:{z_throw, {error, VAR}}).
-define(Z_CATCH(EXPR), ?Z_CATCH(EXPR, undefined)).
-define(Z_CATCH(EXPR, ERROR), try
                                  EXPR
                              catch
                                  _:_ -> throw({z_throw, {error, ERROR}})
                              end).


-import(z_validate, [z_wrap/1, z_wrap/2, z_unwrap/1,
                     z_in_list/2, z_in_list/3,
                     z_proplist_get/2, z_proplist_get/3,
                     z_int_in_range/2, z_int_in_range/3,
                     z_bin_to_int/1, z_bin_to_int/2,
                     z_bin_to_list/1, z_bin_to_list/2,
                     z_bin_to_bool/1, z_bin_to_bool/2,
                     z_bin_to_ex_atom/1, z_bin_to_ex_atom/2,
                     z_only_loweralpha_str/1, z_only_loweralpha_str/2,
                     z_is_email_str/1, z_is_email_str/2,
                     z_is_email_bin/1, z_is_email_bin/2,
                     z_verify/2, z_verify/3,
                     z_apply/2, z_apply/3,
                     z_return/1]).
