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
    case file:make_dir(Path) of
        ok ->
            [10,48,10 | _] = lists:reverse(os:cmd("cd "++ Path ++ 
                " && git init && echo yes | git remote add origin " ++ Url ++ 
                " && echo $?")),
            ok = file:write_file(Path++"/README.MD",list_to_binary(Repository)),
            {ok, _}   = commit(Repository, "initial commit"),
            [10,48,10 | _] = lists:reverse(os:cmd("cd " ++ Path ++ 
                " && git push -u origin master && echo $?")),
            [10,48,10 | _] = lists:reverse(os:cmd("cd " ++ Path ++ " && git "
                "checkout -b devel && git push -u origin devel && echo $?")),
            {ok, list_to_binary(Url)};
        _ ->
            {error, 'name already exists on this account'}
    end.

pull(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    [10,48,10 | Result] = lists:reverse(os:cmd("cd " ++ Path ++ 
        " && git pull && echo $?")),
    {ok, lists:reverse(Result)}.

push(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    [10,48,10 | Result] = lists:reverse(os:cmd("cd " ++ Path ++ 
        " && git push && echo $?")),
    {ok, lists:reverse(Result)}.

commit(Repository, Commit) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    [10,48,10 | Result] = lists:reverse(os:cmd("cd " ++ Path ++ 
        " && git add * && git commit -m \"" ++ Commit ++ "\" -a && echo $?")),
    {ok, lists:reverse(Result)}.

checkout(Repository, Branch) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    [10,48,10 | Result] = lists:reverse(os:cmd("cd " ++ Path ++ 
        " && git checkout " ++ Branch ++ " && echo $?")),
    {ok, lists:reverse(Result)}.

merge(Repository, Branch) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    [10,48,10 | Result] = lists:reverse(os:cmd("cd " ++ Path ++ 
        " && git merge " ++ Branch ++ " && echo $?")),
    {ok, _}   = push(Repository),
    {ok, lists:reverse(Result)}.

log(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    {ok, File} = file:read_file(Path ++ "/.git/logs/HEAD"),
    Logs = binary:split(File, [<<"\n">>], [global]),
    log(Logs,[]).

log([], Result) ->
    {ok, Result};
log([Binary | BinTail], Maps) ->
    [PredCommit, WithoutPredCommit] = binary:split(Bin,[<<" ">>]),
    [Commit, WithoutCommit] = binary:split(WithoutPredCommit,[<<" ">>]),
    [AuthorIncomplete, WithoutAuthor] = binary:split(WithoutCommit,[<<"> ">>]),
    Author = <<AuthorIncomplete/binary, ">">>,
    [DateTime, WithoutDateTime] = binary:split(WithoutAuthor,[<<"\t">>]),
    [Seconds, TimeZone] = binary:split(WithoutAuthor,[<<" ">>]),
    [Action, Description] = binary:split(WithoutDateTime,[<<": ">>]),
    Result = 
        case Action of
            <<"commit", _Rest/binary>> ->
                Map = #{
                    commit      => Commit,
                    author      => Author,
                    time        => Seconds,
                    time_zone   => TimeZone,
                    description => Desc
                },
                [Map | Maps];
            _ ->
                Maps
        end,
    log(BinTail, Result).

%log(Repository) ->
%    {ok, Dir} = application:get_env(gel, repos_dir),
%    Path      = Dir ++ Repository,
%    [10,48,10 | ReversedList] = lists:reverse(os:cmd("cd " ++ Path ++ 
%        " && git log && echo $?")),
%    Binary = list_to_binary(lists:reverse(ReversedList)),
%    [_ | ListOfBinary] = binary:split(<<"\n\n", Binary/binary>>, [<<"\n\ncommit ">>], [global]),
%    bin_to_map(ListOfBinary).

%bin_to_map(Bins) ->
%    bin_to_map(Bins, []).
%bin_to_map([], Maps) ->
%    {ok, Maps};
%bin_to_map([Bin | Bins], Maps) ->
%    [Commit, WithoutCommit] = binary:split(Bin,[<<"\nAuthor: ">>]),
%    [Author, WithoutAuthor] = binary:split(WithoutCommit, [<<"\nDate:   ">>]),
%    [Date, Desc] = binary:split(WithoutAuthor, [<<"\n\n    ">>]),
%    Map = #{
%        commit      => Commit,
%        author      => Author,
%        date        => Date,
%        description => Desc
%    },
%    bin_to_map(Bins, [Map | Maps]).
