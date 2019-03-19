-module(gel_hub).
-author("Kalyta Bogdan").

%% API
-export([new/1]).


%% API
new(Repository) ->
    {ok, Token} = application:get_env(gel, github_access_token),
    Map = #{
        name => Repository, 
        description => <<"Created with Git Erlang Library">>, 
        private => true
    },
    Json = jsx:encode(Map),
    Url = "https://api.github.com/user/repos",
    Headers = [
        {"Authorization", "token " ++ Token}, 
        {"User-Agent", "Gel"}
    ],
    Result = httpc:request(post,{Url, Headers, "application/json", Json},[],[]),
    erlang:display(Result),
    case Result of
        {ok,{{"HTTP/1.1",201,"Created"}, _, ReplyJson}} ->
            ReplyMap = jsx:decode(ReplyJson, [return_maps]),
            Url      = maps:get(<<"html_url">>, ReplyMap),
            {ok, gel:to_list(Url)};
        {ok,{{"HTTP/1.1",422,"Unprocessable Entity"}, _, ReplyJson}} ->
            ReplyMap = jsx:decode(ReplyJson, [return_maps]),
            ErrorMap = maps:get(<<"errors">>, ReplyMap),
            Message  = maps:get(<<"message">>, ErrorMap),
            {error, Message};
        _ ->
            {error, github_error}
    end.
