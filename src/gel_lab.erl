-module(gel_lab).
-author("Kalyta Bogdan").

%% API
-export([new/1]).


%% API
new(Repository) ->
    {ok, Token} = application:get_env(gel, gitlab_access_token),
    Map = #{
        name => list_to_binary(Repository), 
        description => <<"Created with Git Erlang Library">>
    },
    Json = jsx:encode(Map),
    Url = "https://gitlab.com/api/v4/projects?private_token=" ++ Token,
    Headers = [
        {"User-Agent", "Gel"}
    ],
    Opts   = [{body_format, binary}],
    Result = httpc:request(post,{Url,Headers,"application/json",Json},[],Opts),
    case Result of
        {ok,{{"HTTP/1.1",201,"Created"}, _, ReplyJson}} ->
            ReplyMap = jsx:decode(ReplyJson, [return_maps]),
            ReposUrl = maps:get(<<"ssh_url_to_repo">>, ReplyMap),
            {ok, gel:to_list(ReposUrl)};
        {ok,{{"HTTP/1.1",400,"Bad Request"}, _, ReplyJson}} ->
            ReplyMap = jsx:decode(ReplyJson, [return_maps]),
            [ErrorMap] = maps:get(<<"message">>, ReplyMap),
            Message  = maps:get(<<"name">>, ErrorMap),
            {error, Message};
        _ ->
            {error, gitlab_error}
    end.
