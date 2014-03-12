-task({"build:grammar", "Build the URI grammar using abnfc"}).
-task({"clean:grammar", "Delete compiled abnfc grammars"}).
-hook({"build:grammar", run_before, "build:erlang"}).

check("clean:grammar") ->
    tpk_util:check_files_exist(tetrapak:subdir("src"), ".abnf", tetrapak:subdir("src"), ".erl");
check("build:grammar") ->
    tpk_util:check_files_mtime(tetrapak:subdir("src"), ".abnf", tetrapak:subdir("src"), ".erl").

run("build:grammar", Files) ->
    AbnfOptions = [noobj, {o, tetrapak:subdir("src")}],
    lists:foreach(fun ({Input, _}) ->
                      io:format("abnfc ~s~n", [Input]),
                      case abnfc:file(Input, AbnfOptions) of
                          ok      -> ok;
                          Error1  -> tetrapak:fail("compile error: ~p", [Error1])
                      end
                  end, Files);

run("clean:grammar", Files) ->
    [tpk_file:delete(Erl) || {_Abnfc, Erl} <- Files], ok.
