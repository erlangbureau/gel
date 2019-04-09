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
    ok = file:write_file(Path ++ "/.git/HEAD", 
        <<"ref: refs/heads/", (list_to_binary(Branch))/binary>>),
    {ok, BranchHash} = file:read_file(Path ++ "/.git/refs/heads/" ++ Branch),
    <<BranchDir:2/binary, BranchFileName:38/binary, _/binary>> = BranchHash,
    PathBin = list_to_binary(Path),
    {ok, BranchFile} = file:read_file(<<PathBin/binary, "/.git/objects/", BranchDir/binary, "/", BranchFileName/binary>>),
    UnzippedBranch = zlib:uncompress(BranchFile),
    [_Commit, <<"tree ", TreeDir:2/binary, TreeFileName:38/binary, _BranchInfo/binary>>] = binary:split(UnzippedBranch, [<<0>>]),
    {ok, TreeFile} = file:read_file(<<PathBin/binary, "/.git/objects/", TreeDir/binary, "/", TreeFileName/binary>>),
    UnzippedTree = zlib:uncompress(TreeFile),
    [Tree, TreeData] = binary:split(UnzippedTree, [<<0>>]),
    checkout(Tree, TreeData, PathBin, PathBin).

checkout(<<"blob", _Rest/binary>>, Data, _Path, CurrentPath) ->
    ok = file:write_file(CurrentPath, Data),
    ok;
checkout(<<"tree", _TreeId/binary>>, <<>>, _Path, _CurrentPath) ->
    ok;
checkout(<<"tree", TreeId/binary>>, Data, Path, CurrentPath) ->
    ok = filelib:ensure_dir(<<CurrentPath/binary, "/">>),
    [FileId, <<FileHash:20/binary, Rest/binary>>] = binary:split(Data, [<<0>>]),
    <<Dir:2/binary, File:38/binary>> = list_to_binary(lists:flatten([io_lib:format("~2.16.0b",[X]) || <<X:8>> <= FileHash])),
    [_Id, FileName] = binary:split(FileId, [<<" ">>]),
    {ok, ZipFile} = file:read_file(<<Path/binary, "/.git/objects/", Dir/binary, "/", File/binary>>),
    UnzipFile = zlib:uncompress(ZipFile),
    [Type, FileData] = binary:split(UnzipFile, [<<0>>]),
    case file:read_file(<<CurrentPath/binary, "/", FileName/binary>>) of
        {ok, FileRepoData} ->
            CurrentHash = lists:flatten([io_lib:format("~2.16.0b",[X]) || <<X:8>> <= crypto:hash(sha, <<Type/binary, "\0", FileRepoData/binary>>)]),
            case <<Dir:2/binary, File:38/binary>> == CurrentHash of
                true ->
                    ok;
                false ->
                    ok = checkout(Type, FileData, Path, <<CurrentPath/binary, "/", FileName/binary>>)
            end;
        _ ->
            ok = checkout(Type, FileData, Path, <<CurrentPath/binary, "/", FileName/binary>>)
    end,
    checkout(<<"tree", TreeId/binary>>, Rest, Path, CurrentPath).

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
log([<<>> | BinTail], Result) ->
    log(BinTail, Result);
log([Binary | BinTail], Maps) ->
    [_PredCommit, WithoutPredCommit] = binary:split(Binary,[<<" ">>]),
    [Commit, WithoutCommit] = binary:split(WithoutPredCommit,[<<" ">>]),
    [AuthorIncomplete, WithoutAuthor] = binary:split(WithoutCommit,[<<"> ">>]),
    Author = <<AuthorIncomplete/binary, ">">>,
    [DateTime, WithoutDateTime] = binary:split(WithoutAuthor,[<<"\t">>]),
    [Seconds, _TimeZone] = binary:split(DateTime,[<<" ">>]),
    [Action, Description] = binary:split(WithoutDateTime,[<<": ">>]),
    Result = 
        case Action of
            <<"commit", _Rest/binary>> ->
                Map = #{
                    commit      => Commit,
                    author      => Author,
                    time        => Seconds,
                    description => Description
                },
                [Map | Maps];
            _ ->
                Maps
        end,
    log(BinTail, Result).
