%% Geopoly: Geoploy module
%%
-module(geopoly_nif).
-author('fai@code3.io').
-export([user_in_region/4, test/0]).
-on_load(load_nif/0).

user_in_region(Lat, Lng, Polygon_size, Polygon) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

test() -> io:fwrite("It works!\n").

load_nif() ->
    SoName = get_nif_library_path(),
    io:format(<<"Loading library: ~p ~n">>, [SoName]),
    ok = erlang:load_nif(SoName, 0).

get_nif_library_path() ->
    case code:priv_dir(geopoly) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                false ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end.
