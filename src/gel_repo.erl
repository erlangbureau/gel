-module(gel_repo).
-author("Kalyta Bogdan").

%% API
-export([init/1]).
-export([pull/1, push/1]).
-export([commit/2]). 
-export([checkout/2, merge/2]).
-export([log/1]).

%% API
init(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    {ok, Url} = gel_hub:new(Repository),
    Path      = Dir ++ Repository,
    _  = os:cmd("mkdir " ++ Path),
    _  = os:cmd("cd " ++ Path ++ " && git init && "
        "echo yes | git remote add origin " ++ Url),
    ok = file:write_file(Path ++ "/README.MD", list_to_binary(Repository)),
    {ok, _}   = commit(Repository, "initial commit"),
    _  = os:cmd("cd " ++ Path ++ " && git push -u origin master"),
    _  = os:cmd("cd " ++ Path ++ " && git checkout -b devel "
        "&& git push -u origin devel"),
    {ok, list_to_binary(Url)}.

pull(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    Result    = os:cmd("cd " ++ Path ++ " && git pull"),
    {ok, Result}.

push(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    Result    = os:cmd("cd " ++ Path ++ " && git push"),
    {ok, Result}.

commit(Repository, Commit) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    Result    = os:cmd("cd " ++ Path ++ 
        " && git add * && git commit -m \"" ++ Commit ++ "\" -a"),
    {ok, Result}.

checkout(Repository, Branch) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    Result    = os:cmd("cd " ++ Path ++ " && git checkout " ++ Branch),
    {ok, Result}.

merge(Repository, Branch) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    Result    = os:cmd("cd " ++ Path ++ " && git merge " ++ Branch),
    {ok, _}   = push(Repository),
    {ok, Result}.

log(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    Result    = os:cmd("cd " ++ Path ++ " && git log"),
    {ok, Result}.


