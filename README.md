z_validate
==========

Generic validation library for Erlang. "Cool, but how do I use it?" -- good question! 

Example
-------

```erlang
validate_some_input(Input) ->
    try
        %% First, we need to convert our input term to a 'Z-value', sounds
        %% cool right?
        WrappedInput = z_wrap(Input),

        %% Now comes hardcore validation part:
        %% a) make sure we have a 'foo' key in our property list
        %%    and it's a valid integer.
        Foo = z_bin_to_int(z_proplist_get(MaybeInput, {foo}, error_in_foo)),        
        %% b) check that 'Foo' is in a given range, or stop validation 
        %%    with 'foo_not_in_range' error (note the third argument!)
        SmallFoo = z_int_in_range(Foo, {1, 10}),

        %% Just return Foo, unwrapping it from a 'Z-value' (yeah, this 
        %% does look ugly -- we're working on it :)
        z_return(z_unwrap(SmallFoo))
    catch
        ?Z_OK(Result)   -> {ok, Result};
        ?Z_ERROR(Error) -> {error, Error}
    end.
```

As you've probably guessed already:

* any term you `z_return()` will end up in `?Z_OK(...)` clause of the 
  try-catch block;
* any validation error will arrive in `?Z_ERROR()` clause, with
  `Error` bound to that tricky third argument of every `z_anything`
  function ('error_in_foo' in our case).