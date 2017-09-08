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

-module(float_sizes).

-export([main/1]).

-define(ENCODERS, [
   {"jiffy", fun(X) -> iolist_size(jiffy:encode(list_to_float(X))) end},
   {"erl", fun(X) -> couch_ejson_size1:encoded_size(list_to_float(X)) end},
   {"jsone", fun(X) -> iolist_size(jsone:encode(list_to_float(X))) end},
   {"jsx", fun(X) -> iolist_size(jsx:encode(list_to_float(X))) end},
   {"python", fun python_encoded_size/1},
   {"node", fun node_encoded_size/1}
]).


-define(INPUTS, [
    "0.0",
    "0.1",
    "0.2",
    "0.9",
    "0.01",
    "0.001",
    "0.00001",
    "0.002",
    "0.0002",
    "0.000002",
    "0.11",
    "0.111",
    "0.1111",
    "0.3333333",
    "0.33333333",
    "0.90000009",
    "0.900000009",
    "0.99999999",
    "0.999999999",
    "0.100000000000000017",
    "0.10000000000000000018",
    "1.0",
    "1.1",
    "20.02",
    "300.003",
    "4000.0004",
    "50000.00005",
    "600000.000006",
    "10000000000.0",
    "11111111111.01",
    "99999999999.99999999",
    "3.3333332538604736328125",
    "3.3333333",
    "2.000002",
    "2.0000002",
    "2.00000002",
    "2.0000057",
    "2.0000057220458984375",
    "1.0e1",
    "1.0e10",
    "1.000001e10",
    "1.0000001e10",
    "1.000001e16",
    "1.1e16",
    "1.0e25",
    "1.0e30",
    "1.00000000001e29",
    "1.00000000001e30",
    "1.0e100",
    "1.0e308",
    "2.718281828459045235",
    "3.141592653589793238",
    "3.141592653589793238e100",
    "1.1443742e-28",
    "3.0316488e-13",
    "2.7670118E19",
    "1.0e-10",
    "1.0e-30",
    "1.000000001e-30",
    "1.0e-100",
    "1.0e-200",
    "1.0e-308"
]).


main(_) ->
    Res = [[In] ++ [EncFun(In) || {_, EncFun} <- ?ENCODERS] || In <- ?INPUTS],
    pp(Res, ?ENCODERS),
    ok.

python_encoded_size(Val) ->
    EncodedOutput = cmd("/usr/bin/env python -c '"
        "import json; print json.dumps(" ++ Val ++ ")"
        "'"),
    length(EncodedOutput).


node_encoded_size(Val) ->
    EncodedOutput = cmd("/usr/bin/env node -e '"
       "console.log(JSON.stringify(" ++ Val ++ "));"
       "'"),
    length(EncodedOutput).


cmd(Cmd) ->
    string:strip(os:cmd(Cmd), right, $\n).


pp(Res, Encoders) ->
    EncNames = [Name || {Name, _} <- Encoders],
    Cols = length(EncNames),
    EncColumsFmt = "~24.. s |" ++ string:copies("~7.. s |", Cols) ++ "~n",
    io:format(EncColumsFmt, ["Input"] ++ EncNames),
    io:format("~s~n", [string:copies("-", 80)]),
    NumColumnsFmt = string:copies("~7.. B |", Cols),
    LineFmt = "~24.. s |" ++ NumColumnsFmt ++ "~n",
    [io:format(LineFmt, LineVals) || LineVals <- Res],
    ok.
