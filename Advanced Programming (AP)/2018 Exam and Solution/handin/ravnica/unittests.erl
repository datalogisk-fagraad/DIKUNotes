-module(unittests).
-include_lib("eunit/include/eunit.hrl").

-on_load(setup/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% How to run tests from current folder:       %%
%%  (1) in erl, type 'c(unittests).' and enter %%
%%  (2) type 'unittests:test().'               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

setup() ->
  compile:file(district),
  ok.

create_test() -> {A, _} = district:create("A"), [?assertEqual(ok, A)].

% Get description from district under configuration
get_description1_test() ->
  Expected = "test",
  {ok, A} = district:create(Expected),
  {ok, Actual} = district:get_description(A),
  [?assertEqual(Expected, Actual)].

% Get description from active district
get_description2_test() ->
  Expected = "test",
  {ok, A} = district:create(Expected),
  active = district:activate(A),
  {ok, Actual} = district:get_description(A),
  [?assertEqual(Expected, Actual)].

connect_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  Return = district:connect(A, t, B),
  Return = district:connect(B, t, C),
  [?assertEqual(ok, Return)].

active_simple_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(B, t, C),
  Return = district:activate(A),
  [?assertEqual(active, Return)].

active_selfloop_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(B, t, C),
  ok = district:connect(A, t2, A),
  Return = district:activate(A),
  [?assertEqual(active, Return)].

active_cycle_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(B, t, C),
  ok = district:connect(C, t, A),
  Return = district:activate(A),
  [?assertEqual(active, Return)].

active_cycleandloop_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(B, t, C),
  ok = district:connect(C, t, A),
  ok = district:connect(A, t1, A),
  Return = district:activate(A),
  [?assertEqual(active, Return)].

active_double_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(B, t, C),
  ok = district:connect(C, t, A),
  ok = district:connect(A, t1, A),
  Return = district:activate(A),
  [?assertEqual(active, Return)].

actions1_test() -> 
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(A, t1, A),
  Return = district:options(A),
  [?assertEqual({ok, [t, t1]}, Return)].

actions2_test() -> 
  {ok, A} = district:create("Test"),
  Return = district:options(A),
  [?assertEqual({ok, []}, Return)].

actions3_test() -> 
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  ok = district:connect(A, a, B),
  ok = district:connect(B, b, A),
  ok = district:connect(A, aa, A),
  active = district:activate(A),
  Return1 = district:options(A),
  Return2 = district:options(B),
  [?assertEqual({ok, [a, aa]}, Return1), ?assertEqual({ok, [b]}, Return2)].

enter1_test() -> 
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  ok = district:connect(A, a, B),
  ok = district:connect(B, b, A),
  ok = district:connect(A, aa, A),
  Creature = {make_ref(), #{}},
  Return = district:enter(A, Creature),
  Msg = "Under configuration. No creatures are allowed to enter.",
  [?assertEqual({error, Msg}, Return)].

enter2_test() -> 
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  ok = district:connect(A, a, B),
  ok = district:connect(B, b, A),
  ok = district:connect(A, aa, A),
  active = district:activate(A),
  Creature = {make_ref(), #{}},
  Return = district:enter(A, Creature),
  [?assertEqual(ok, Return)].

enter3_test() -> 
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  ok = district:connect(A, a, B),
  ok = district:connect(B, b, A),
  ok = district:connect(A, aa, A),
  active = district:activate(A),
  Creature = {make_ref(), #{}},
  Return1 = district:enter(A, Creature),
  Return2 = district:enter(A, Creature),
  Msg = "A creature is already in the district.",
  [ ?assertEqual(ok, Return1)
  , ?assertEqual({error, Msg}, Return2)].

take_action1_test() -> 
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  ok = district:connect(A, a, B),
  ok = district:connect(B, b, A),
  active = district:activate(A),
  {CRef, _} = Creature = {make_ref(), #{}},
  ok = district:enter(A, Creature),
  ok = district:enter(B, Creature),
  Return = district:take_action(A, CRef, a),
  Msg = "Creature could not enter new district.",
  [ ?assertEqual({error, Msg}, Return)].

take_action2_test() -> 
  {ok, A} = district:create("A"),
  {ok, B} = district:create("B"),
  ok = district:connect(A, a, B),
  ok = district:connect(B, b, A),
  active = district:activate(A),
  {CRef, _} = Creature = {make_ref(), #{}},
  ok = district:enter(A, Creature),
  Return = district:take_action(A, CRef, a),
  Me = self(),
  ok = district:shutdown(B, Me),
  receive
    {_, {shutting_down, From, Cs}} -> 
      ?assertEqual(From, B),
      ?assertEqual(Cs, [Creature])
  end,

  [ ?assertEqual({ok, B}, Return)].

take_action3_test() -> 
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  ok = district:connect(A, a, B),
  ok = district:connect(B, b, A),
  active = district:activate(A),
  {CRef, _} = Creature = {make_ref(), #{}},
  ok = district:enter(A, Creature),
  ok = district:enter(B, Creature),
  Return = district:take_action(A, CRef, c),
  Msg = "Either action or creature does not exist here.",
  [ ?assertEqual({error, Msg}, Return)].

take_action4_test() -> 
  {ok, A} = district:create("Test"),
  ok = district:connect(A, a, A),
  active = district:activate(A),
  {CRef, _} = Creature = {make_ref(), #{}},
  ok = district:enter(A, Creature),
  Return = district:take_action(A, CRef, a),
  [ ?assertEqual({ok, A}, Return)].

shutdown_simple_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  {ok, NextPlane} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(B, t, C),
  Return1 = district:activate(A),
  Return2 = district:shutdown(A, NextPlane),
  try district:get_description(A),
      error
  catch exit:_ -> 
    [ ?assertEqual(active, Return1)
    , ?assertEqual(ok, Return2)
    ]
  end.

shutdown_cycle_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  {ok, NextPlane} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(B, t, C),
  ok = district:connect(C, t, A),
  ok = district:connect(A, t1, A),
  Return1 = district:activate(A),
  Return2 = district:shutdown(A, NextPlane),
  try district:get_description(A),
      error
  catch exit:_ -> 
    [ ?assertEqual(active, Return1)
    , ?assertEqual(ok, Return2)
    ]
  end.

shutdown_selfloop_test() ->
  {ok, A} = district:create("Test"),
  {ok, B} = district:create("Test"),
  {ok, C} = district:create("Test"),
  {ok, NextPlane} = district:create("Test"),
  ok = district:connect(A, t, B),
  ok = district:connect(B, t, C),
  ok = district:connect(C, t, A),
  ok = district:connect(A, t1, A),
  Return1 = district:activate(A),
  Return2 = district:shutdown(A, NextPlane),
  try district:get_description(A),
      error
  catch exit:_ -> 
    [ ?assertEqual(active, Return1)
    , ?assertEqual(ok, Return2)
    ]
  end.