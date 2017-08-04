% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_ebench).


-export([main/1]).


-define(INT_LARGE, trunc(42.0e100)).
-define(FLOAT_17D,  0.100000000000000017).
-define(FLOAT_1D, 0.2).
-define(FLOAT_LARGE, 1.0e200).
-define(HWAIR, $\x{10348}).  % 4 byte utf8 encoding
-define(EURO, $\x{20ac}).  % 3 byte utf8 encoding
-define(CENT, $\x{a2}).  % 2 byte utf8 encoding

-define(ENCODERS, [
   {"jiffy", fun(X) -> iolist_size(jiffy:encode(X)) end},
   {"erlfast", fun couch_ejson_size1:encoded_size/1},
   {"erlexact", fun couch_ejson_size2:encoded_size/1},
   {"jsone", fun(X) -> iolist_size(jsone:encode(X)) end}
]).

-define(INPUTS, [
    {"nested_arr", fun(N) -> arr_nested(N) end},
    {"nested_obj", fun(N) -> obj_nested(N) end},
    {"int_small_arr", fun(N) -> arr(N, 42) end},
    {"int_large_arr", fun(N) -> arr(N, ?INT_LARGE) end},
    {"float_1d_arr", fun(N) -> arr(N, ?FLOAT_1D) end},
    {"float_17d", fun(N) -> arr(N, ?FLOAT_17D) end},
    {"float_large_arr", fun(N) -> arr(N, ?FLOAT_LARGE) end},
    {"str_basic", fun(N) -> str(N, $x) end},
    {"str_escn", fun(N) -> str(N, $\n) end},
    {"str_ctr1", fun(N) -> str(N, $\x{1}) end},
    {"str_hwair", fun(N) -> str(N, ?HWAIR) end},
    {"str_euro", fun(N) -> str(N, ?EURO) end},
    {"str_cent", fun(N) -> str(N, ?CENT) end},
    {"file_1", fun(N) -> get_file(N, "1.json") end}
]).


main(Args) when is_list(Args) ->
    %io:setopts([{encoding, unicode}]),
    Sizes = [arg_to_int(A) || A <- Args],
    log("Generating inputs..."),
    Inputs = get_inputs(Sizes),
    Encoders = ?ENCODERS,
    warmup(Inputs, Encoders),
    log("Running benchmark: ~b encoders x ~b tests x input size(s): ~w",
        [length(Encoders), length(Inputs), Sizes]),
    Res = bench(Inputs, Encoders),
    log("Finished running benchmark."),
    pp(Res),
    ok.


warmup(Inputs, Encoders) ->
    [[EncFun(InputV) || {_, InputV} <- Inputs] || {_, EncFun} <- Encoders],
    ok.


bench({InputSize, _}, InputVal, EncFun) when is_function(EncFun) ->
    N = get_tries(InputSize),
    Tries = lists:seq(1, N),
    {TimeUSec, Result} = timer:tc(fun() -> hd([EncFun(InputVal) || _ <- Tries]) end),
    PerCallTimeUSec  = TimeUSec / N,
    {PerCallTimeUSec, Result}.


bench(Inputs, Encoders) when is_list(Inputs), is_list(Encoders) ->
    [
        {EncName, [begin
            {TimeUSec, Result} = bench(InputDesc, InputVal, EncFun),
            {InputDesc, TimeUSec, Result}
        end || {InputDesc, InputVal} <- Inputs]}
    || {EncName, EncFun} <- Encoders].


get_tries(Size) when Size =< 100 ->
    1000;

get_tries(Size) when Size =< 1000 ->
    100;

get_tries(Size) when Size =< 10000 ->
    10;

get_tries(_Size)  ->
    1.

arg_to_int(Arg) when is_atom(Arg) ->
    arg_to_int(atom_to_list(Arg));

arg_to_int(Arg) when is_list(Arg) ->
    list_to_integer(Arg).


%% Reporting

pp(Results) ->
    io:format("Enc.  | Input size | Input type   | USec       | Bytes ~n"),
    io:format("------+------------+--------------+------------+----------~n"),
    [pp_encoder_result(EncResult) || EncResult <- Results],
    ok.


pp_encoder_result({EncName, Results}) ->
    io:format("~n~s~n", [EncName]),
    [pp_line(Result) || Result <- Results],
    ok.


pp_line({{InputSize, InputName}, Time, Result}) ->
    Fmt = "      |  ~9.. B | ~12.. s |~11.1. f | ~10.. B~n",
    io:format(Fmt, [InputSize, InputName, Time, Result]).


%% Test input generation

get_inputs(Sizes) when is_list(Sizes) ->
    lists:sort(lists:flatten([get_input(S) || S <- Sizes])).


get_input(Size) when is_integer(Size) ->
    [{{Size, Name}, F(Size)} || {Name, F} <- ?INPUTS].


get_file(Name) ->
    Path = "priv/" ++ Name,
    {ok, Contents} = file:read_file(Path),
    jiffy:decode(Contents).


get_file(1, Name) ->
    get_file(Name);

get_file(N, Name) when N > 1 ->
    Obj = get_file(Name),
    [Obj || _ <- lists:seq(1, N)].


arr_nested(MaxDepth) ->
    arr_nested(MaxDepth, 0).


obj_nested(MaxDepth) ->
    obj_nested(MaxDepth, 0).


arr(N, V) ->
    [V || _ <- lists:seq(1, N)].


obj(N, K, V) ->
    {[{K, V} || _ <- lists:seq(1, N)]}.


str(N, C) ->
    unicode:characters_to_binary([C || _ <- lists:seq(1, N)]).


arr_nested(MaxDepth, MaxDepth) ->
    [];

arr_nested(MaxDepth, Depth) ->
    [arr_nested(MaxDepth, Depth + 1)].


obj_nested(MaxDepth, MaxDepth) ->
    obj(1, <<"k">>, <<"v">>);

obj_nested(MaxDepth, Depth) ->
    {[{<<"k">>, obj_nested(MaxDepth, Depth + 1)}]}.


log(Line) ->
    log(Line, []).

log(Format, Args) ->
    io:format(Format ++ "~n", Args).
