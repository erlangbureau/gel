-module(gel_repo).
-author("Kalyta Bogdan").

-include_lib("kernel/include/file.hrl").

%% API
%%  cmd
-export([init/2]).
-export([pull/1, push/1]).
-export([commit/2]). 
-export([merge/2]).
%%  completed
-export([checkout/2]).
-export([log/1]).

%% API
%%  cmd
init(Repository, Readme) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    {ok, Url} = gel_hub:new(Repository),
    Path      = Dir ++ Repository,
    case file:make_dir(Path) of
        ok ->
            ListInit = os:cmd("cd "++ Path ++ 
                " && git init && echo yes | git remote add origin " ++ Url ++ 
                " && echo OK || echo Failed"),
            case lists:suffix("Failed", ListInit) of
                false ->
                    ok = file:write_file(Path++"/README.MD",list_to_binary(Readme)),
                    {ok, _}   = commit(Repository, "initial commit"),
                    ListPush = os:cmd("cd " ++ Path ++ 
                        " && git push -u origin master && echo OK || echo Failed"),
                    case lists:suffix("Failed", ListPush) of
                        false ->
                            ListCheckout = lists:reverse(os:cmd("cd " ++ Path ++
                                " && git checkout -b devel && git push -u origin devel"
                                " && echo OK || echo Failed")),
                            case lists:suffix("Failed", ListCheckout) of
                                false ->
                                    {ok, list_to_binary(Url)};
                                true ->
                                    {error, ListCheckout}
                            end;
                        true ->
                            {error, ListPush}
                    end;
                true ->
                    {error, ListInit}
            end;
        _ ->
            {error, 'name already exists on this account'}
    end.

pull(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    ListPull = os:cmd("cd " ++ Path ++ " && git pull && echo OK || echo Failed"),
    case lists:suffix("Failed", ListPull) of
        false ->
            {ok, ListPull};
        true ->
            {error, ListPull}
    end.

push(Repository) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    ListPush = os:cmd("cd " ++ Path ++ 
        " && git push && echo OK || echo Failed"),
    case lists:suffix("Failed", ListPush) of
        false ->
            {ok, ListPush};
        true ->
            {error, ListPush}
    end.

commit(Repository, Commit) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    ListCommit = lists:reverse(os:cmd("cd " ++ Path ++ 
        " && git add * && git commit -m \"" ++ Commit ++ "\" -a"
        " && echo OK || echo Failed"),
    case lists:suffix("Failed", ListCommit) of
        false ->
            {ok, ListCommit};
        true ->
            {error, ListCommit}
    end.

merge(Repository, Branch) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    ListMerge = os:cmd("cd " ++ Path ++ 
        " && git merge " ++ Branch ++ " && echo OK || echo Failed"),
    {ok, _}   = push(Repository),
    case lists:suffix("Failed", ListMerge) of
        false ->
            {ok, ListMerge};
        true ->
            {error, ListMerge}
    end.

%%  completed
checkout(Repository, Branch) ->
    {ok, Dir} = application:get_env(gel, repos_dir),
    Path      = Dir ++ Repository,
    PathBin   = list_to_binary(Path),
    ok        = file:write_file(Path ++ "/.git/HEAD", 
        <<"ref: refs/heads/", (list_to_binary(Branch))/binary>>),
    {ok, BranchHash} = file:read_file(Path ++ "/.git/refs/heads/" ++ Branch),
    <<BranchDir:2/binary, BranchFileName:38/binary, _/binary>> = BranchHash,
    {ok, BranchFile} = file:read_file(<<PathBin/binary, "/.git/objects/", 
        BranchDir/binary, "/", BranchFileName/binary>>),
    UnzippedBranch   = zlib:uncompress(BranchFile),
    [_C, <<"tree ", TreeDir:2/binary, TreeFileName:38/binary, _BInfo/binary>>] =
        binary:split(UnzippedBranch, [<<0>>]),
    {ok, TreeFile}   = file:read_file(<<PathBin/binary, "/.git/objects/", 
        TreeDir/binary, "/", TreeFileName/binary>>),
    UnzippedTree     = zlib:uncompress(TreeFile),
    [Tree, TreeData] = binary:split(UnzippedTree, [<<0>>]),
    {ok, OldFiles}   = file:list_dir(Path),
    ok               = delete_old_files(Path, lists:delete(".git", OldFiles)),
    {ok, FilesData}  = checkout(Tree, TreeData, PathBin),
    update_index(<<TreeDir/binary, TreeFileName/binary>>, FilesData, PathBin).
            
checkout(Type, Data, Path) ->
    checkout(Type, Data, Path, Path, []).

checkout(<<"blob", _Rest/binary>>, Data, _Path, CurrentPath, Acc) ->
    ok = file:write_file(CurrentPath, <<Data/binary>>),
    {ok, Acc};
checkout(<<"tree", _TreeId/binary>>, <<>>, _Path, _CurrentPath, Acc) ->
    {ok, Acc};
checkout(<<"tree", TreeId/binary>>, Data, Path, CurrentPath, Acc) ->
    ok = filelib:ensure_dir(<<CurrentPath/binary, "/">>),
    [FileId, <<FileHash:20/binary, Rest/binary>>] = binary:split(Data, [<<0>>]),
    <<Dir:2/binary, File:38/binary>> = list_to_binary(
        lists:flatten([io_lib:format("~2.16.0b",[X]) || <<X:8>> <= FileHash])),
    [_Id, FileName] = binary:split(FileId, [<<" ">>]),
    FilePath = <<Path/binary, "/.git/objects/", Dir/binary, "/", File/binary>>,
    {ok, ZipFile} = file:read_file(FilePath),
    {ok, FileInfo} = file:read_file_info(FilePath),
    UnzipFile = zlib:uncompress(ZipFile),
    [Type, FileData] = binary:split(UnzipFile, [<<0>>]),
    Name = case Type of
        <<"blob", _Rest/binary>> ->
            [_, N] = binary:split(<<CurrentPath/binary, "/", FileName/binary>>,
                [<<Path/binary, "/">>]),
            N;
        <<"tree", _TreeId/binary>> ->
            FileName
    end,
    Map = #{
        type => Type,
        ctime => calendar:datetime_to_gregorian_seconds(FileInfo#file_info.ctime) - 62167219200,
        mtime => calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime) - 62167219200,
        size => byte_size(FileData),
        inode => FileInfo#file_info.inode,
        uid => FileInfo#file_info.uid,
        gid => FileInfo#file_info.gid,
        device => FileInfo#file_info.major_device,
        mode => FileInfo#file_info.mode,
        sha => <<Dir/binary, File/binary>>, 
        name => Name, 
        path => CurrentPath
    },
%    case file:read_file(<<CurrentPath/binary, "/", FileName/binary>>) of
%        {ok, FileRepoData} ->
%            CurrentHash = lists:flatten(
%               [io_lib:format("~2.16.0b",[X]) || <<X:8>> <= crypto:hash(sha, 
%               <<Type/binary, "\0", FileRepoData/binary>>)]), %% Type = "blob " + byte_size(FileRepoData)
%            case <<Dir:2/binary, File:38/binary>> == CurrentHash of
%                true ->
%                    ok;
%                false ->
    {ok, NewAcc} = checkout(Type, FileData, Path, 
        <<CurrentPath/binary, "/", FileName/binary>>, [Map | Acc]),
%            end;
%        _ ->
%            ok = checkout(Type, FileData, Path, <<CurrentPath/binary, "/", FileName/binary>>)
%    end,
    checkout(<<"tree", TreeId/binary>>, Rest, Path, CurrentPath, NewAcc).

log(Repository) ->
    {ok, Dir}  = application:get_env(gel, repos_dir),
    Path       = Dir ++ Repository,
    {ok, File} = file:read_file(Path ++ "/.git/logs/HEAD"),
    Logs       = binary:split(File, [<<"\n">>], [global]),
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

%% internal
delete_old_files(_, []) ->
    ok;
delete_old_files(Path, [File | Tail]) ->
    case file:delete(Path ++ "/" ++ File) of
        ok -> 
            ok;
        {error, eperm} -> 
            {ok, OldFiles} = file:list_dir(Path ++ "/" ++ File),
            ok = delete_old_files(Path ++ "/" ++ File, OldFiles),
            ok = file:del_dir(Path ++ "/" ++ File)
    end,
    delete_old_files(Path, Tail).

update_index(TreeSHA, FilesData, PathBin) ->
    {ok, BinFiles, Dirs, Paths} = file_info_to_binary(FilesData),
    {ok, BinDirs, DirsAtTree} = dir_info_to_binary(lists:reverse(Dirs), Paths),
    DirsCount = integer_to_binary(maps:get(PathBin, DirsAtTree, 0)),
    SHA = << <<(list_to_integer([H],16)):4>> || H <- binary_to_list(TreeSHA) >>,
    DirLen = byte_size(BinDirs) + 25,
    Bin = <<"DIRC", 00,00,00,02, (length(Paths)):32>>, 
    BinWithFiles = add_files(Bin, BinFiles),
    NewBin = <<
        BinWithFiles/binary, 
        "TREE", DirLen:32, 
        00, (integer_to_binary(length(Paths)))/binary, 32, DirsCount/binary, 10, 
        SHA/binary, 
        BinDirs/binary
    >>,
    CheckSum = crypto:hash(sha, NewBin),
    file:write_file(<<PathBin/binary,"/.git/index">>, 
        <<NewBin/binary, CheckSum/binary>>).

file_info_to_binary(FilesData) ->
    file_info_to_binary(FilesData, [], [], []).

file_info_to_binary([], Files, Dirs, Paths) ->
    {ok, Files, Dirs, Paths};
file_info_to_binary([Map | Tail], Files, Dirs, Acc) ->
    case maps:get(type, Map) of
        <<"tree", _TreeId/binary>> ->
            file_info_to_binary(Tail, Files, [Map | Dirs], Acc);
        <<"blob", _Rest/binary>> ->
            CTime  = <<(maps:get(ctime, Map)):32, 00,00,00,00>>,
            MTime  = <<(maps:get(mtime, Map)):32, 00,00,00,00>>,
            Device = <<(maps:get(device, Map)):32>>,
            Inode  = <<(maps:get(inode, Map)):32>>,
            Mode   = <<(maps:get(mode, Map)):32>>,
            UID    = <<(maps:get(uid, Map)):32>>,
            GID    = <<(maps:get(gid, Map)):32>>,
            Size   = <<(maps:get(size, Map)):32>>,
            FName  = maps:get(name, Map),
            Name   = round_byte(<<00, (byte_size(FName)):8, FName/binary, 00>>),
            SHA    = << << (list_to_integer([H], 16)):4 >> || H <- binary_to_list(maps:get(sha, Map)) >>,
            Bin    = <<
                CTime/binary, MTime/binary, 
                Device/binary, Inode/binary, Mode/binary, 
                UID/binary, GID/binary, 
                Size/binary, SHA/binary, Name/binary
            >>,
            Path = maps:get(path, Map),
            file_info_to_binary(Tail, [Bin | Files], Dirs, [Path | Acc])
    end.

dir_info_to_binary(DirsData, Paths) ->
    dir_info_to_binary(DirsData, <<>>, Paths, maps:new()).

dir_info_to_binary([], Dirs, _Paths, CounterMap) ->
    {ok, Dirs, CounterMap};
dir_info_to_binary([Map | Tail], Dirs, FilePaths, CounterMap) ->
    Name = maps:get(name, Map),
    Path = maps:get(path, Map),
    FileCount = length(lists:filter(
        fun (K) -> 
            binary:match(K, <<Path/binary, "/", Name/binary>>) /= nomatch 
        end, 
        FilePaths)),
    DirCount = maps:get(<<Path/binary, "/", Name/binary>>, CounterMap, 0),
    SHA      = << << (list_to_integer([H], 16)):4 >> || H <- binary_to_list(maps:get(sha, Map)) >>,
    NewDirs  = <<Name/binary, 00, (integer_to_binary(FileCount))/binary, 32, 
        (integer_to_binary(DirCount))/binary, 10, SHA/binary, Dirs/binary>>,
    NewCounter = maps:put(Path, maps:get(Path, CounterMap, 0) + 1, CounterMap),
    dir_info_to_binary(Tail, NewDirs, FilePaths, NewCounter).

add_files(Bin, []) ->
    Bin;
add_files(OldBin, [File | Tail]) ->
    Bin = <<OldBin/binary, File/binary>>,
    NewBin = 
        case (byte_size(Bin) rem 8) /= 0 of
            true  ->
                Bin;
            false -> 
                <<Bin/binary, 00,00,00,00>>
        end,
    add_files(NewBin, Tail).

round_byte(Bin) when (byte_size(Bin) rem 4) == 0 ->
    Bin;
round_byte(Bin) ->
    round_byte(<<Bin/binary, 00>>).
