%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for test.

-module(test_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
	ets:new(urls, [named_table, public]),
	
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
	Method = Req:get(method),
	
	io:format("~p /~s~n", [Method, Path]),
	
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
	
            case filelib:is_file(filename:absname(Path, DocRoot)) of
                true ->
                    Req:serve_file(Path, DocRoot);
				false ->
					handle_get(Req)
            end;
        'POST' ->
            case Path of
				[] -> handle_post(Req);
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

handle_post(Req) ->
	LastId = ets:info(urls, size) + 10000,
	
	{Url, _} = get_option("url", Req:parse_post()),
	case Url of
		"http://" ++ Url1 ->
			ets:insert(urls, {LastId, Url});
		_ -> 
			ets:insert(urls, {LastId, "http://" ++ Url})
	end,
	YourId = io_lib:fwrite("~.36b", [LastId]),
	Req:respond({200, [], "Your Url Is:  http://localhost/" ++ YourId})
	.

handle_get(Req) ->	
	"/" ++ Path = Req:get(path),
	
	case io_lib:fread("~36u", Path) of
		{ok, [Id], _} ->
			
			case ets:lookup(urls, Id) of
				[{Id, Url}] -> 
					Req:respond({302, [{"Location", Url}], ""});
				_ -> Req:respond({404, [], "No such key"})
			end;
		_ -> 
			Req:respond({500, [], "We are sorry, something went wrong!"})
		
	end.
	
%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
