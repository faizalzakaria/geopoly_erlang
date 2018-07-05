%% Geopoly: Geoploy module
%%
-module(geopoly_nif).
-author('fai@code3.io').
-export([user_in_region/4, hello_world/0]).
-on_load(load_nif/0).

%% With a large Bin argument, exor/2 and exor_bad take far too long for a NIF
user_in_region(lat, lng, polygon_size, polygon) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

hello_world() -> io:fwrite("hello, world\n").

load_nif() ->
    SoName = get_nif_library_path(),
    io:format(<<"Loading library: ~p ~n">>, [SoName]),
    ok = erlang:load_nif(SoName, 0).

get_nif_library_path() ->
    case code:priv_dir(geopolylib) of
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
