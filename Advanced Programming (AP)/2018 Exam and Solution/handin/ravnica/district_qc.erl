-module(district_qc).

-export([ex/0, territory/0, setup_territory/1]).
-export([prop_activate/0, prop_take_action/0]).

-include_lib("eqc/include/eqc.hrl").
% Choose between SIZE different districts
-define(SIZE, 10).

% ok, so I run quickcheck tests by running the command:
% eqc:quickcheck(district_qc:territory())

% Used for connections between districts.
atom() -> elements([a,b,c,d,e,f,g,h,i,j,k,l]).

% DOCUMENTATION ON ?LET
% Generates a value from G1, binds it to Pat, then generates a value from G2 
% (which may refer to the variables bound in Pat).


% Generate integet between 1 and SIZE
integer() -> choose(1, ?SIZE).

% Makes a list [{atom(), integer()}] of variable length; expected length about 5
connections() ->
    ?LAZY(
        frequency([ {1, []}
                  , {5, ?LETSHRINK([L], [connections()],
                      {call, lists, keystore, 
                        ?LETSHRINK([K], 
                            [atom()], [K, 1, L, {K, integer()}])})}
                  ])
    ).

territory() ->
    ?LAZY(
        frequency([ {1, {call,maps,new,[]}}
                  , {4, ?LETSHRINK([M], [territory()],
                      {call, maps, put, [integer(), connections(), M]})}
                  ])
).

ex() -> #{
    1 => [{a, 2}, {d, 4}, {h, 1}],
    2 => [{b, 3}, {d, 4}],
    3 => [{c, 1}, {d, 4}, {l, 99}]
}.

connectToNeighbours(From, A, L) ->
    lists:foldl(fun({Action, IntTo}, A_To) -> 
                % Is the neighbour already created?
                IsToCreated = maps:is_key(IntTo, A_To),
                % if so, we just connect and go on
                if IsToCreated -> To = maps:get(IntTo, A_To),
                                  district:connect(From, Action, To),
                                  A_To;
                   % otherwise, we have to create the neighbour before connect
                   true -> {ok, To} = district:create("District " ++ IntTo),
                           % return the map, where we add the neighbour as well
                           maps:put(IntTo, To, A_To)
                end
    end, A, L).

setup_territory(InputMap) -> 
    % The accumulator is a map, pairing each integer with a district
    Map = maps:fold(fun(IntFrom, L, A_From) -> 
        % Check if we have already created the district D
        IsFromCreated = maps:is_key(IntFrom, A_From),
        % if so, we find its pid and go fold through its neighbours
        if IsFromCreated ->
            % The district id associated with the district integer 
            From = maps:get(IntFrom, A_From),
            connectToNeighbours(From, A_From, L);

           % This is the case if we have not already created From
           true ->
               % Create the district 
               {ok, From} = district:create("District " ++ IntFrom),
               % Add the district to the map
               APlus = maps:put(IntFrom, From, A_From),
               connectToNeighbours(From, APlus, L)
        end
    end, #{}, InputMap),
    maps:values(Map).

prop_activate() ->
    % We want to check that for all active districts, 
    % any neighbour for such a given district is active as well.
    % However, since this is done by taking action -> see prop_take_action()
    % This property just tests that all districts can be activated
    ?FORALL(Xs, territory(),
        begin
            Eval = eval(Xs),
            % First, setup the world
            World = setup_territory(Eval),
            lists:all(fun(District) -> 
                Check = district:activate(District),
                if (Check == active) or (Check == under_activation) -> 
                    true;
                   true -> false
                end
            end, World)
        end
    ).

prop_take_action() ->
    ?FORALL(Xs, territory(),
        begin
            Eval = eval(Xs),
            % First, setup the world
            World = setup_territory(Eval),
            lists:all(fun(District) -> 
                Check = district:activate(District),
                if (Check == active) or (Check == under_activation) -> 
                    {ok, Actions} = district:options(District),
                    lists:foreach(fun(Action) -> 
                        Creature = {CRef, _} = {make_ref(), #{}},
                        ok = district:enter(District, Creature),
                        {ok, _} = district:take_action(District, CRef, Action)
                    end, Actions),
                    true;
                   true -> false
                end
            end, World)
        end
    ).
