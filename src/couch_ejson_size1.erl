-module(couch_ejson_size1).

-export([encoded_size/1]).


%% Compound objects

encoded_size({[]}) ->
    2;  % opening { and closing }

encoded_size({Props}) ->
    % 2 is because opening { and closing }
    % Inside the lc 2 is for : and , but counts an extra , at end
    % -1 is to subtract last , from lc part
    2 + lists:sum([encoded_size(K) + encoded_size(V) + 2 || {K, V} <- Props]) - 1;

encoded_size([]) ->
    2;  % opening [ and closing ]

encoded_size(List) when is_list(List) ->
    % 2 is for [ and ]
    % inside the lc 1 is for , but it counts one extra , for last element
    2 + lists:sum([encoded_size(V) + 1 || V <- List]) - 1;

%% Floats. Start with some common values

encoded_size(0.0) ->
    3;

encoded_size(1.0) ->
    3;

encoded_size(0.1) ->
    3;

encoded_size(Float) when is_float(Float), Float < 0.0 ->
    encoded_size(-Float) + 1;

encoded_size(Float) when is_float(Float), Float > 1.0e-8, Float < 1.0e16 ->
    byte_size(float_to_binary(Float, [{decimals, 14}, compact]));

encoded_size(Float) when is_float(Float) ->
    try
        byte_size(float_to_binary(Float, [{scientific, 14}, compact]))
    catch
        error:badarg ->  % large values can raise a badarg
            5  % 1eXXX
    end;

%% Integers

encoded_size(0) ->
    1; % log(0) is not defined

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
    2 + byte_size(Binary); %+ MatchCount;

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
