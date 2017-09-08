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

-module(couch_ejson_size1).

-export([encoded_size/1]).


%% Compound objects

encoded_size({[]}) ->
    2;  % opening { and closing }

encoded_size({KVs}) ->
    % Would add 2 because opening { and closing }, but then inside the LC
    % would accumulate an extra , at the end so subtract 2 - 1
    1 + lists:sum([encoded_size(K) + encoded_size(V) + 2 || {K,V} <- KVs]);

encoded_size([]) ->
    2;  % opening [ and closing ]

encoded_size(List) when is_list(List) ->
    % 2 is for [ and ] but inside LC would accumulate an extra , so subtract
    % 2 - 1
    1 + lists:sum([encoded_size(V) + 1 || V <- List]);

%% Floats. Start with some common values

encoded_size(0.0) ->
    3;

encoded_size(1.0) ->
    3;

encoded_size(0.1) ->
    3;

encoded_size(Float) when is_float(Float), Float < 0.0 ->
    encoded_size(-Float) + 1;

encoded_size(Float) when is_float(Float), Float > 1.0e-16, Float < 1.0e16 ->
    byte_size(float_to_binary(Float, [{decimals, 10}, compact]));

encoded_size(Float) when is_float(Float), Float > 1.0e-50, Float < 1.0e50 ->
    byte_size(float_to_binary(Float, [{scientific, 10}, compact]));

encoded_size(Float) when is_float(Float), Float > 1.0e-100, Float < 1.0e100 ->
    5;

encoded_size(Float) when is_float(Float) ->
    6;

%% Integers

encoded_size(0) ->
    1;

encoded_size(Integer) when is_integer(Integer), Integer < 0 ->
    encoded_size(-Integer) + 1;

encoded_size(Integer) when is_integer(Integer), Integer < 10 ->
    1;

encoded_size(Integer) when is_integer(Integer), Integer < 100 ->
    2;

encoded_size(Integer) when is_integer(Integer), Integer < 1000 ->
    3;

encoded_size(Integer) when is_integer(Integer) ->
    trunc(math:log10(Integer)) + 1;

%% Strings

encoded_size(Binary) when is_binary(Binary) ->
    2 + byte_size(Binary);

%% Special terminal symbols as atoms

encoded_size(null) ->
    4;

encoded_size(true) ->
    4;

encoded_size(false) ->
    5;

%% Other atoms

encoded_size(Atom) when is_atom(Atom) ->
    encoded_size(atom_to_binary(Atom, utf8)).
