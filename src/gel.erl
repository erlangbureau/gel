-module(gel).
-author("Kalyta Bogdan").

%% API
-export([init/1]).
-export([pull/1, push/1]).
-export([commit/2]).
-export([checkout/2, merge/2]).
-export([log/1]).

%% API
init(Repository) ->
    gel_repo:init(to_list(Repository)).

pull(Repository) ->
    gel_repo:pull(to_list(Repository)).

push(Repository) ->
    gel_repo:push(to_list(Repository)).

commit(Repository, Commit) ->
    gel_repo:commit(to_list(Repository), to_list(Commit)).

checkout(Repository, Branch) ->
    gel_repo:checkout(to_list(Repository), to_list(Branch)).

merge(Repository, Branch) ->
    ListRepository = to_list(Repository),
    ListBranch     = to_list(Branch),
    {ok, _} = checkout(ListRepository, "master"),
    {ok, Result} = gel_repo:merge(ListRepository, ListBranch),
    {ok, _} = checkout(ListRepository, ListBranch),
    {ok, Result}.

log(Repository) ->
    gel_repo:log(to_list(Repository)).

%% internal
to_list(List) when is_list(List) ->
    List;
to_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom).
