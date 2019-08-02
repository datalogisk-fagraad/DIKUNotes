-module(district).
-behaviour(gen_statem).

-export([create/1,
         get_description/1,
         connect/3,
         activate/1,
         options/1,
         enter/2,
         take_action/3,
         shutdown/2,
         trigger/2]).

% gen_statem callback module: init, callback_mode and terminate
-export([init/1, callback_mode/0, terminate/3]).

% gen_statem state functions
-export([ under_configuration/3
        , active/3
        , under_activation/3
        , shutting_down/3
        , activationsubroutine/3
        , shutdownsubroutine/3
        , kill_yourself/3
        ]).

%%%%%%%%%%%%%%%%%%%%%
% CALLBACK FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%

%TODO: remove io:write statements

% Callback mode:
% state_functions are used, where events are handled by one function per state
callback_mode() -> [state_functions, state_enter].

% Init:
% Takes a district description as input and returns
% ok, as a confirmation code that the operation was successful
% under_configuration atom, denoting that the district is under construction
% a state, containing 
%   (1) a description of the disctrict, 
%   (2) a map of connections,
%   (3) a triggerlist
init(Desc) -> 
    Connections = maps:new(),
    Trigger = none,
    {ok, 
     under_configuration, 
     {Desc, Connections, Trigger}
    }.

terminate(_Reason, _StateName, _StateData) -> void.
    % io:fwrite("Termination was reached.~n", []).

%%%%%%%%%%%%%%%%%%%%%
% Ravnica Client API
%%%%%%%%%%%%%%%%%%%%%

-type passage() :: pid().
-type creature_ref() :: reference().
-type creature_stats() :: map().
-type creature() :: {creature_ref(), creature_stats()}.
-type trigger() :: fun((entering | leaving, creature(), [creature()])
                     -> {creature(), [creature()]}).

-spec create(string()) -> {ok, passage()} | {error, any()}.
create(Desc) -> gen_statem:start_link(?MODULE, Desc, []).

% Call the specified district using the atom getDescription,
% denoting to the genstate district that it should send back a description
-spec get_description(passage()) -> {ok, string()} | {error, any()}.
get_description(District) -> gen_statem:call(District, getDescription).


% Call the specified district From, requesting a connection to To with Action
-spec connect(passage(), atom(), passage()) -> ok | {error, any()}.
connect(From, Action, To) -> gen_statem:call(From, {connect, Action, To}).

% Try to activate the input district
-spec activate(passage()) -> active | under_activation | impossible.
activate(District) -> gen_statem:call(District, activate).

-spec options(passage()) -> {ok, [atom()]} | none.
options(District) -> gen_statem:call(District, getOptions).

-spec enter(passage(), creature()) -> ok | {error, any()}.
enter(District, Creature) -> gen_statem:call(District, {enter, Creature}).

-spec take_action(passage(), creature_ref(), atom()) -> 
    {ok, passage()} | {error, any()}.
take_action(District, CRef, Action) -> 
    gen_statem:call(District, {takeAction, CRef, Action}).

-spec shutdown(passage(), pid()) -> ok.
shutdown(District, NextPlane) -> internalshutdown(District, NextPlane, []).

internalshutdown(District, NextPlane, Ds) ->
    ok = gen_statem:call(District, {shutdown, NextPlane, Ds}),
    % io:fwrite("shutdown: ok, someone told me they're shutdown. EXTERMINATE!~n", []),
    gen_statem:stop(District),
    ok.

-spec trigger(passage(), trigger()) -> ok | {error, any()} | not_supported.
trigger(District, Trigger) -> gen_statem:call(District, {trigger, Trigger}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions used internally by the districts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Used for shutting down subroutines
endsubprocess(Process) -> gen_statem:stop(Process).

% Removes self loops. Used in the shutting down process
removeLoops(Cons, D) -> maps:filter (fun(_K, V) -> V /= D end, Cons).

% subroutine used for shutting down neighbours
shutdownSubroutine(Subprocess, Neighbours, Ds, NextPlane) -> 
    gen_statem:call(Subprocess, {shutdownsubroutine, Neighbours, Ds, NextPlane}).

% Folds through a list of connections and activates.
actfolder({_Action, District}, R) ->
    if R == impossible -> impossible;
       true -> What = activate(District),
            %    io:fwrite("We got: ~p~n", [What]),
               What
    end.

% Folds through a list of connections and shuts down.
shutfolder(Ds, {_Action, District}, {ok, NextPlane}) ->
    % First, try to shut down the neighbour
    try
        IsInDs = lists:member(District, Ds),
        if IsInDs -> {ok, NextPlane};
           true -> {internalshutdown(District, NextPlane, Ds), NextPlane}
        end
    catch
        % Well, maybe the neighbour is already shutdown, in which case this
        % results in an exit error. In that case, we know that the neighbour
        % is already shut down (or at least non-existing).
        exit:_Reason  -> {ok, NextPlane}
    end.

%%%%%%%%%%%%%%%%%%%
% STATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%

% ACTIVATION SUBROUTINE STATE
activationsubroutine(enter, _OldState, Data) -> 
    % io:fwrite("we're now in the subroutine state. nice?~n", []),
    {Master, _Desc, Connections, _Trigger} = Data,
    % Get all neighbours, and for each of them, call the activate subroutine
    Neighbours = maps:to_list(Connections),
    % io:fwrite("Neighbours:~p~n", [Neighbours]),
    % Ask all neighbours in turn to activate
    % io:fwrite("Subroutine: Calling folder.~n", []),
    Result = lists:foldl(fun(A, B) -> actfolder(A, B) end, active, Neighbours),
    % Reply back to the master process with the result
    % io:fwrite("Subroutine: OK, so now I'm done folding. Result is: ~p~n", [Result]),
    {keep_state_and_data, [{reply, Master, Result}]};

% Any other interaction with subroutine is futile
% This is assumed not to happen
activationsubroutine(_, _, _) -> keep_state_and_data.

% SHUTDOWN SUBROUTINE STATE
shutdownsubroutine(enter, _OldState, Data) -> 
    % io:fwrite("we're now in the subroutine state. nice?~n", []),
    {Master, NextPlane, Ds, _Desc, Connections, _Trigger} = Data,
    % Get all neighbours, and for each of them, call the activate subroutine
    Neighbours = maps:to_list(Connections),
    % io:fwrite("Neighbours:~p~n", [Neighbours]),
    % Ask all neighbours in turn to activate
    % io:fwrite("Subroutine: Calling folder.~n", []),
    {ok, _} = lists:foldl(fun(A, B) -> shutfolder(Ds, A, B) end, {ok, NextPlane}, Neighbours),
    % Reply back to the master process with the result
    % io:fwrite("Subroutine: OK, so now I'm done folding. Result is: ~p~n", [Result]),
    {keep_state_and_data, [{reply, Master, ok}]};

% Any other interaction with subroutine is futile
% This is assumed not to happen
shutdownsubroutine(_, _, _) -> keep_state_and_data.

% UNDER CONFIGURATION STATE

% State enter call (unused, ignore)
under_configuration(enter, _OldState, _Data) -> keep_state_and_data;

% Special case used only for activation in subroutines
under_configuration({call, From}, {activationsubroutine, Connections}, Data) ->
    % io:fwrite("Im the subroutine call. Gonna go into subroutine state.~n", []),
    {Desc, _OldConnections, Trigger} = Data,
    NewData = {From, Desc, Connections, Trigger},
    % We're gonna go into the subroutine state,
    % remembering who the caller/master process is
    {next_state, activationsubroutine, NewData, [{reply, From, ok}]};

% Special case used only for shutting down in subroutines
under_configuration({call, From}, {shutdownsubroutine, Cons, Ds, P}, Data) ->
    % io:fwrite("Im the subroutine call. Gonna go into subroutine state.~n", []),
    {Desc, _OldConnections, Trigger} = Data,
    NewData = {From, P, Ds, Desc, Cons, Trigger},
    % We're gonna go into the subroutine state,
    % remembering who the caller/master process is
    {next_state, shutdownsubroutine, NewData, [{reply, From, ok}]};

% Under configuration
% TODO: maybe this can give an error(?) handle??
% handling description requests
under_configuration({call, From}, getDescription, Data) ->
    {Desc, _Connections, _Trigger} = Data,
    {keep_state_and_data, [{reply, From, {ok, Desc}}]};

% Handling connections
under_configuration({call, From}, {connect, Action, To}, Data) ->
    % First, declare data to access the connections
    {Desc, Connections, Trigger} = Data,
    % Check if the action is already used for a connection
    ActionIsAlreadyUsed = maps:is_key(Action, Connections),
    if ActionIsAlreadyUsed ->  
        % If it is, then return error without changing anything
        Msg = "Action is already in use.", 
        {keep_state_and_data, [{reply, From, {error, Msg}}]};
       true ->
        % Otherwise, add the link to the connections and return 'ok'
        % We anticipate activation by denoting a bool for activity
        NewConnections = maps:put(Action, To, Connections),
        NewData = {Desc, NewConnections, Trigger},
        {keep_state, NewData, [{reply, From, ok}]}
    end;

% Someone is trying to get action options from this state, which is ok
under_configuration({call, From}, getOptions, Data) ->
    {_Desc, Connections, _Trigger} = Data,
    Actions = maps:keys(Connections),
    {keep_state_and_data, [{reply, From, {ok, Actions}}]};

% Someone is erroneusly trying to put a creature in a shutting down district!
under_configuration({call, From}, {enter, _Creature}, _Data) ->
    Msg = "Under configuration. No creatures are allowed to enter.",
    {keep_state_and_data, [{reply, From, {error, Msg}}]};

% Someone is erroneusly trying to take action in a shutting down district!
under_configuration({call, From}, {takeAction, _CRef, _Action}, _Data) -> 
    Msg = "Under configuration. No actions are allowed.",
    {keep_state_and_data, [{reply, From, {error, Msg}}]};

% Someone treats this district as the 'Next Plane'
under_configuration(cast, {shutting_down, _D, _Cs}, _) -> 
    % io:fwrite("NextPlane: Huh? Why do I get all these creatures??.~n", []),
    keep_state_and_data;

% Someone is adding a trigger to the district
under_configuration({call, From}, {trigger, Trigger}, Data) -> 
    {Desc, Cons, _OldTrigger} = Data,
    % Overwrite whatever trigger we had stored, and return 'ok'
    NewData = {Desc, Cons, Trigger},
    {keep_state, NewData, {reply, From, ok}};

% Ok, we're being shut down. Commence.
under_configuration({call, From}, {shutdown, NextPlane, Ds}, Data) ->
    % io:fwrite("Ok, let the shutdown begin.~n", []),
    % Pattern match on data
    {Desc, Cons, Trigger} = Data,
    % Ok, since we already have a subprocess going on, we're just gonna kill it
    NewData = {From, Desc, Cons, [], Trigger, NextPlane, Ds, noSubProcess},
    {next_state, shutting_down, NewData};

% Someone is activating us! So we should go ahead and activate
under_configuration({call, From}, activate, Data) ->
    % Pattern match on data
    {Desc, Connections, Trigger} = Data,
    % Remember who called for activation.
    NewStateData = {From, Desc, Connections, Trigger, noSubProcess},
    % Change state, since we're now under activation
    {next_state, under_activation, NewStateData}.

% UNDER ACTIVATION STATE

activateSubroutine(Subprocess, Neighbours) -> 
    gen_statem:call(Subprocess, {activationsubroutine, Neighbours}).

% This is where we initially try to activate all the neighbours of the district
under_activation(enter, _OldState, Data) ->
    % io:fwrite("we're now under activation~n", []),
    % Pattern match on data to get connections
    {_Caller, _Desc, Connections, _Trigger, _SubProcess} = Data,
    % Spawn a process, technically a district, that has
    % this district's neighbours, and which takes care of activating neighbours
    {ok, SubDistrict} = create("This is a subdistrict, used for activation."),
    % io:fwrite("Master: subroutine created.~n", []),
    ok = activateSubroutine(SubDistrict, Connections),
    % Then, go on, and wait for answers (while also answering queries)!
    NewData = {_Caller, _Desc, Connections, _Trigger, SubDistrict},
    {keep_state, NewData};

% This happens when someone is trying to figure out if we are active or not
under_activation({call, From}, activate, _Data) ->
    % io:fwrite("Someone just asked me if a was active. I am under activation.~n", []),
    % We just tell them that we're under activation and that's it
    {keep_state_and_data, [{reply, From, under_activation}]};

% handling initialization call
under_activation({call, From}, getDescription, Data) ->
    {_Caller, Desc, _Connections, _Trigger, _SubProcess} = Data,
    {keep_state_and_data, [{reply, From, {ok, Desc}}]};

% Handling connections
under_activation({call, From}, {connect, _Action, _To}, _Data) ->
    Msg = "District is under activation, and connections can not be formed.",
    {keep_state_and_data, [{reply, From, {ok, Msg}}]};

% Someone is trying to get action options from this state, which is not ok!
under_activation({call, From}, getOptions, _Data) ->
    {keep_state_and_data, [{reply, From, none}]};

% Someone is erroneusly trying to put a creature in a shutting down district!
under_activation({call, From}, {enter, _Creature}, _Data) ->
    Msg = "Under activation. No creatures are allowed to enter.",
    {keep_state_and_data, [{reply, From, {error, Msg}}]};

% Someone is erroneusly trying to take action in a district under activation!
under_activation({call, From}, {takeAction, _CRef, _Action}, _Data) -> 
    Msg = "Under activation. No actions are allowed.",
    {keep_state_and_data, [{reply, From, {error, Msg}}]};

% Someone is sending us creatures??
under_activation(cast, {shutting_down, _D, _Cs}, _) -> keep_state_and_data;

% Someone is erroneusly adding a trigger to the district
under_activation({call, From}, {trigger, _Trigger}, _Data) ->
    Msg = "You cannot add a trigger to a district under activation.",
    {keep_state_and_data, {reply, From, {error, Msg}}};

% This matches the case where we get the result back from the subroutine
under_activation(info, {_Ref, Result}, Data) ->
    % io:fwrite("Master: ERHMAGHERD?? I gotz something: ~p~n", [Result]),
    {Caller, Desc, Connections, Trigger, SubProcess} = Data,
    % OK, we need to be able to model creatues, which we store in a map
    Creatures = [],
    NewData = {Desc, Connections, Creatures, Trigger},
    % End the subprocess, since its work is now done
    endsubprocess(SubProcess),
    if Result == impossible ->
        % It was impossible to activate the district!
        % In that case, we revert back to being under configuration
        NewData = {Desc, Connections, Trigger},
        {next_state, under_configuration, NewData, [{reply, Caller, Result}]};
       true -> {next_state, active, NewData, [{reply, Caller, active}]}
    end;

% Ok, we're being shut down. Commence.
under_activation({call, From}, {shutdown, NextPlane, Ds}, Data) ->
    % io:fwrite("Ok, let the shutdown begin.~n", []),
    % Pattern match on data
    {Caller, Desc, Cons, Trigger, SubProcess} = Data,
    % Ok, since we already have a subprocess going on, we're just gonna kill it
    NewData = {From, Desc, Cons, [], Trigger, NextPlane, Ds, noSubProcess},
    try
        endsubprocess(SubProcess),
        % Change state, since we're now under shutdown
        % Also, notify caller that activation is impossible
        {next_state, shutting_down, NewData, [{reply, Caller, impossible}]}

    catch
        _ -> % Change state, since we're now under shutdown
             {next_state, shutting_down, NewData, [{reply, Caller, impossible}]}
    end.
    

trigRun(From, Trigger, Event, Creature, Creatures) -> 
    % io:fwrite("trigRun: Hello!. Gonna try to eval Trigger~n", []),
    Me = self(),
    try {_, _} = Result = Trigger(Event, Creature, Creatures),
        % io:fwrite("trigRun: Trigger evaluated.~n", []),
        From ! {Me, Result}
    catch 
        _ -> From ! {Me, error}
    end.

creaturesCheck(Old, New) ->
    % same length; no new creatures have risen
    SameLength = length(Old) == length(New),
    % check that for all creatures in the old list,
    % there is a creature in the new list with the same ref
    % if the lists are same length, this mapping is one to one
    Same = lists:all (fun({R1, _}) -> 
                lists:any(fun({R2, _}) -> R1 == R2 end, New) 
            end, Old),
    % return the combined boolean
    Same and SameLength.

% sanity check that the result of the trigger is actually well formed
sanitycheck(Result, Creature, Creatures) -> 
    % try to pattern match with creature tuple
    try {{Ref, _}, NewCreatures} = Result,
        % get creature ref
        {CRef, _} = Creature,
        % check refs for all remaining creatures
        Same = creaturesCheck(Creatures, NewCreatures),
        % these should all be the same
        Same and (CRef == Ref)
    catch _ -> false
    end.

% We use runTrigger to evaluate the trigger of a given district
runTrigger(Trigger, Event, Creature, Creatures) ->
    Me = self(),
    % We don't want to crash, so we spawn a function that does the work for us
    % It might potentially crash, but we wait at most 2 seconds, in which case
    % we return the original input, and thus the program survives
    Pid = spawn(fun() -> trigRun(Me, Trigger, Event, Creature, Creatures) end),
    receive
        % wait for a response from the worker and sanity check its answer
        {Pid, Result} -> SanityCheck = sanitycheck(Result, Creature, Creatures),
                         % if it passes the check, return the result
                         if SanityCheck -> Result;
                            % otherwise give back original creatures
                            true -> {Creature, Creatures}
                         end
    % do no wait for more than 2 seconds for a response, though
    after 2000 -> 
        io:fwrite("District: Not gonna wait on the trigger anymore.~n", []),
        {Creature, Creatures}
    end.

% ACTIVE STATE

% A creature enters the dungeon!
active({call, From}, {enter, Creature}, Data) ->
    % First we match the data and the creature
    {Desc, Connections, Creatures, Trigger} = Data,
    % io:fwrite("~p: A creature enters the dungeon!~n", [Desc]),
    {CRef, _Stats} = Creature,
    Event = entering,
    % check if the creature is already in the district
    % IsKey = maps:is_key(Ref, Creatures),
    % io:fwrite("Creatures now looks like: ~p~n", [Creatures]),
    IsKey = lists:keymember(CRef, 1, Creatures),
    % io:fwrite(".. and here lol??~n", []),
    % If so, return error
    if IsKey -> Msg = "A creature is already in the district.",
                {keep_state_and_data, [{reply, From, {error, Msg}}]};
       % otherwise, we can add the creature
       true -> 
           % if there's no trigger, then we just go ahead
           if Trigger == none ->
                    %   io:fwrite("No trigger here. Move along.~n", []),
                      NewCreatures = lists:append([Creature], Creatures),
                      NewData = {Desc, Connections, NewCreatures, Trigger},
                    %   io:fwrite("The creature list is now: ~p~n", [NewCreatures]),
                      {keep_state, NewData, [{reply, From, ok}]};
              % otherwise, we trigger the trigger and use the result
              true -> {C, Cs} = runTrigger(Trigger, Event, Creature, Creatures),
                    %   {CRef, CStats} = C,
                      NewCreatures = lists:append([C], Cs),
                      NewData = {Desc, Connections, NewCreatures, Trigger},
                      {keep_state, NewData, [{reply, From, ok}]}
            end
    end;

% State enter call (unused, ignore)
active(enter, _OldState, _Data) -> keep_state_and_data;

% handling description requests
% You can get description in all states
active({call, From}, getDescription, Data) ->
    {Desc, _Connections, _Creatures, _Trigger} = Data,
    {keep_state_and_data, [{reply, From, {ok, Desc}}]};

% Handling connections
active({call, From}, {connect, _Action, _To}, _Data) ->
    Msg = "District is active, and connections can not be formed.",
    {keep_state_and_data, [{reply, From, {ok, Msg}}]};

% Someone is trying to activate us
active({call, From}, activate, _Data) ->
    % We just tell them that we're activated, and that's it
    {keep_state_and_data, [{reply, From, active}]};

% Someone is taking an action in the district
active({call, From}, {takeAction, CRef, Action}, Data) -> 
    % So, first we pattern match on the state data
    {Desc, Connections, Creatures, Trigger} = Data,
    % then we check if the specified creature is actually here
    IsCreature = lists:keymember(CRef, 1, Creatures),
    % we also check that the specified action is available
    IsAction = maps:is_key(Action, Connections),

    % If the creature is in the district AND the action is valid
    if IsCreature and IsAction ->
        % we find the destination 
        To = maps:get(Action, Connections),
        % and the creature
        Creature = lists:keyfind(CRef, 1, Creatures),
        % Stats = maps:get(CRef, Creatures),
        Me = self(),
        % Now, we have to check if any trigger messes with the creatures
        % if there's no trigger, then we just go ahead
        if Trigger == none ->
          % First we try to skip the creature off to the next district
          % If its a self-loop, don't bother
          if Me == To ->
             {keep_state_and_data, [{reply, From, {ok, To}}]};
             true -> S = enter(To, Creature),
               if S == ok -> 
                 % we remove it from the current district and reply back
                 NewCreatures = lists:keydelete(CRef, 1, Creatures),
                 NewData = {Desc, Connections, NewCreatures, Trigger},
                 {keep_state, NewData, [{reply, From, {ok, To}}]};
                 % on the other hand, if it went wrong, then we report error
                 % and keep the creature here
                 true -> Msg = "Creature could not enter new district.",
                         {keep_state_and_data, [{reply, From, {error, Msg}}]}
               end
          end;
          % if this went well,

           % otherwise, we trigger the trigger
           true -> 
             % We run the trigger and try to send the creature off
             {C, Cs} = runTrigger(Trigger, leaving, Creature, Creatures),
              % If we have a self-loop, then just keep him here
              % After running trigger-enter
              if Me == To -> 
                 {C2, Cs2} = runTrigger(Trigger, entering, C, Cs),
                 NewCreatures = [C2 | Cs2],
                 NewData = {Desc, Connections, NewCreatures, Trigger},
                 {keep_state, NewData, [{reply, From, {ok, To}}]};
               true ->
                S = enter(To, C),
                % if this went well, we keep the new creatues and return {ok, To}
                if S == ok -> 
                   NewCreatures = Cs,
                   NewData = {Desc, Connections, NewCreatures, Trigger},
                   {keep_state, NewData, [{reply, From, {ok, To}}]};
                   % on the other hand, if it went wrong, then we report error
                   % and keep the creature here, with NO changes to any creature
                 true -> Msg = "Creature could not enter new district.",
                         {keep_state_and_data, [{reply, From, {error, Msg}}]}
                end
              end
        end;
       true -> Msg = "Either action or creature does not exist here.",
               {keep_state_and_data, [{reply, From, {error, Msg}}]}
    end;

% Someone is erroneusly adding a trigger to the district
active({call, From}, {trigger, _Trigger}, _Data) ->
    Msg = "You cannot add a trigger to an active district.",
    {keep_state_and_data, {reply, From, {error, Msg}}};

% Someone is trying to get action options from this state, which is ok
active({call, From}, getOptions, Data) ->
    {_Desc, Connections, _Creatures, _Trigger} = Data,
    Actions = maps:keys(Connections),
    {keep_state_and_data, [{reply, From, {ok, Actions}}]};

% Someone is giving us creatures? Ignore!
active(cast, {shutting_down, _D, _Cs}, _) -> keep_state_and_data;

% Ok, we're being shut down. Commence.
active({call, From}, {shutdown, NextPlane, Ds}, Data) ->
    % io:fwrite("Ok, let the shutdown begin.~n", []),
    % Pattern match on data
    {Desc, Cons, Creatures, Trigger} = Data,
    % Ok, since we already have a subprocess going on, we're just gonna kill it
    NewData = {From, Desc, Cons, Creatures, Trigger, NextPlane, Ds, noSubProcess},
    {next_state, shutting_down, NewData}.

% SHUTTING DOWN STATE

% State enter call for shutting down. Things happen here!
shutting_down(enter, _OldState, Data) ->
    % io:fwrite("we're now under shutdown~n", []),
    % Pattern match on data to get connections
    {Caller, Desc, Connections, Creatures, Trigger, NextPlane, Ds, _} = Data,   
    % send creatures to NextPlane
    Me = self(),
    gen_statem:cast(NextPlane, {shutting_down, Me, Creatures}),
    % Spawn a process, technically a district, that has
    % this district's neighbours, and which takes care of activating neighbours
    {ok, SubDistrict} = create("This is a subdistrict, used for shutdown."),
    % io:fwrite("Master: subroutine created.~n", []),

    % remove selfloops from connections before handing them over
    NoLoopsBrother = removeLoops(Connections, Me),
    % We also tell the subroutine to NOT kill me, or anyone on the Ds list
    NewDs = lists:append([self()], Ds),
    ok = shutdownSubroutine(SubDistrict, NoLoopsBrother, NewDs, NextPlane),
    % io:fwrite("Master: Activated subroutine.~n", []),
    % Get all neighbours, and for each of them, call the activate subroutine
    % Neighbours = maps:to_list(Connections),
    % Each entry has format {Key, {Action, District, IsActive}}
    % lists:foreach(fun({_, D, _}) -> activateSubroutine(D) end , Neighbours),
    % Then, go on, and wait for answers!
    NewData = { Caller
              , Desc
              , Connections
              , Creatures
              , Trigger
              , NextPlane
              , Ds
              , SubDistrict},
    % keep state, but save caller and subproces
    {keep_state, NewData};

% This matches the case where we get the result back from the subroutine
shutting_down(info, {_Ref, ok}, Data) ->
    % io:fwrite("Got response.. Data is: ~p~n", [Data]),
    {Caller, _, _, _, _, _, _, SubProcess} = Data,
    gen_statem:stop(SubProcess),
    % goto one final state, where you just end yourself
    {next_state, kill_yourself, [], [{reply, Caller, ok}]};

% handling description requests
% You can get description in all states
shutting_down({call, From}, getDescription, Data) ->
    {_Caller, Desc, _Connections, _Creatures, _Trigger, _NextPlane, _Ds, _} = Data,
    {keep_state_and_data, [{reply, From, {ok, Desc}}]};

% Someone is trying to get action options from this state, which is not ok!
shutting_down({call, From}, getOptions, _Data) ->
    {keep_state_and_data, [{reply, From, none}]};

% Someone is erroneusly adding a trigger to the district
shutting_down({call, From}, {trigger, _Trigger}, _Data) ->
    Msg = "You cannot add a trigger to a district that is shutting down.",
    {keep_state_and_data, {reply, From, {error, Msg}}};

% Someone is erroneusly trying to put a creature in a shutting down district!
shutting_down({call, From}, {enter, _Creature}, _Data) ->
    Msg = "District is shutting down. No creatures are allowed to enter.",
    {keep_state_and_data, [{reply, From, {error, Msg}}]};

% Someone is erroneusly trying to take action in a shutting down district!
shutting_down({call, From}, {takeAction, _CRef, _Action}, _Data) -> 
    Msg = "District is shutting down. No actions are allowed.",
    {keep_state_and_data, [{reply, From, {error, Msg}}]};

% This happens when someone is trying to shut us down
shutting_down({call, From}, {shutdown, _NextPlane, _Ds}, _Data) ->
    % We just tell them that we're already shutting down and that's it
    % io:fwrite("Someone asked me if I was shutting down. I am.~n", []),
    % io:fwrite("I don't think that someone should ask me that?~n", []),
    {keep_state_and_data, [{reply, From, ok}]};

shutting_down(cast, {shutting_down, _D, _Creatures}, _) -> keep_state_and_data;

% Handling connections
shutting_down({call, From}, {connect, _Action, _To}, _Data) ->
    Msg = "District is shutting down, and connections can not be formed.",
    {keep_state_and_data, [{reply, From, {ok, Msg}}]}.

kill_yourself(enter, _OldState, _Data) -> 
    % io:fwrite("Hark hark, dying here..~n", []),
    keep_state_and_data.