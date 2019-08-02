% Example contributed by Joachim and Mathias
-module(triggertest).
-export([test/0]).

make_drunker({CreateRef, Stats}) ->
    #{sobriety := CurSobriety} = Stats,
    {CreateRef, Stats#{sobriety := CurSobriety - 1}}.

make_sober({CreateRef, Stats}) ->
    #{sobriety := CurSobriety} = Stats,
    {CreateRef, Stats#{sobriety := CurSobriety + 1}}.

cheers(_, Creature, Creatures) ->
    io:format("Cheeeeers!~n"),
    {make_drunker(Creature), lists:map(fun make_drunker/1, Creatures)}.

rest_a_bit(entering, Creature, Creatures) ->
    io:format("Sob..~n"),
    {make_sober(Creature), Creatures};
rest_a_bit(leaving, Creature, Creatures) ->
    {Creature, Creatures}.

andrzejs_office(entering, {CreatureRef, Stats}, Creatures) ->
    io:format("You get lost in Andrzejs stacks of papers, lose 1 sanity!~n"),
    #{sanity := CurSanity} = Stats,
    {{CreatureRef, Stats#{sanity := CurSanity - 1}}, Creatures};
andrzejs_office(leaving, Creature, Creatures) ->
    io:format("Someone is leaving Andrzejs office!~n"),
    {Creature, Creatures}.

lille_up1(entering, {CreatureRef, Stats}, Creatures, KenRef, AndrzejRef) ->
    CreatureRefs = lists:map(fun({Ref, _Stats}) -> Ref end, Creatures),
    KenPresent = lists:member(KenRef, CreatureRefs),
    AndrzejPresent = lists:member(AndrzejRef, CreatureRefs),
    if KenPresent and AndrzejPresent ->
        io:format("Surprise! Ken and Andrzej are here!~n"),
        {{CreatureRef, Stats#{stunned => true}}, Creatures};
       true ->
        {{CreatureRef, Stats}, Creatures}
    end;
lille_up1(leaving, _Creature, _Creatures, _KenRef, _AndrzejRef) ->
    io:format("Someone is leaving LilleUP1! This trigger should fail.~n"),
    % This is misbehaving, thus the trigger has no effect
    ok.

generate_territory() ->
    {ok, KensOffice} = district:create("Ken's office"),
    {ok, AndrzejsOffice} = district:create("Andrzej's office"),
    {ok, CoffeeMachine} =
        district:create("The Coffee Machine at the end of the PLTC hallway"),
    {ok, Canteen} =
        district:create("The Canteen at the top floor of the DIKU building"),
    {ok, Cafeen} = district:create("The student bar, \"Cafeen?\""),
    {ok, Bathroom} = district:create("The bathroom at the student bar"),
    {ok, LilleUP1} =
        district:create("The smaller auditorium at the DIKU building"),

    ok = district:connect(KensOffice, restore_health, CoffeeMachine),
    ok = district:connect(AndrzejsOffice, prepare_attack, CoffeeMachine),

    % Andrzej sometimes skips his coffee
    ok = district:connect(AndrzejsOffice, sneak, LilleUP1),

    ok = district:connect(CoffeeMachine, surprise_attack, LilleUP1),
    ok = district:connect(Canteen, make_haste, Cafeen),
    ok = district:connect(Canteen, have_courage, LilleUP1),
    ok = district:connect(LilleUP1, rejuvenate, Canteen),

    ok = district:connect(Cafeen, try_to_leave, Cafeen),
    ok = district:connect(Cafeen, need_to_pee, Bathroom),
    ok = district:connect(Bathroom, go_back, Cafeen),

    % Places to spawn or place advanced triggers
    [KensOffice, AndrzejsOffice, CoffeeMachine, Canteen, Bathroom,
     Cafeen, LilleUP1].

place_triggers(KenRef, AndrzejRef, AndrzejsOffice, Cafeen,
               Bathroom, LilleUP1) ->
    district:trigger(AndrzejsOffice, fun andrzejs_office/3),
    district:trigger(Cafeen, fun cheers/3),
    district:trigger(Bathroom, fun rest_a_bit/3),
    district:trigger(LilleUP1,
           fun (Event, Creature, Creatures) ->
              lille_up1(Event, Creature, Creatures, KenRef, AndrzejRef)
           end),
    ok.

test() ->
    KenRef = make_ref(),
    AndrzejRef = make_ref(),

    KenStats = #{hp => 100, sanity => 7.4},
    AndrzejStats = #{hp => 100, sanity => 80, mana => 100},

    [KensOffice, AndrzejsOffice, _CoffeeMachine, Canteen, Bathroom,
     Cafeen, LilleUP1] = generate_territory(),

    place_triggers(KenRef, AndrzejRef, AndrzejsOffice, Cafeen,
                   Bathroom, LilleUP1),

    % Activate the initial nodes. The rest will follow
    active = district:activate(KensOffice),
    active = district:activate(AndrzejsOffice),
    active = district:activate(Canteen),

    Ken = {KenRef, KenStats},
    Andrzej = {AndrzejRef, AndrzejStats},

    StudentRefs = lists:map(fun (_) -> make_ref() end, lists:seq(1, 100)),
    StudentStats = #{hp => 10, sobriety => 50, sanity => 15},

    PrebenRef = make_ref(),
    PrebenStats = #{hp => 1, sobriety => 150, sanity => 150},

    % io:fwrite("run_world(): We now enter, triggering triggers.~n", []),
    % Spawn the creatures
    ok = district:enter(KensOffice, Ken),
    ok = district:enter(AndrzejsOffice, Andrzej),
    ok = district:enter(Cafeen, {PrebenRef, PrebenStats}),
    lists:map(fun (StudentRef) ->
                      ok = district:enter(Canteen, {StudentRef, StudentStats})
              end, StudentRefs),


    % io:fwrite("run_world(): We now take action, triggering triggers.~n", []),
    % =====| Following two lines changed in ver. 1.0.1 | =====
    {ok, _} = district:take_action(KensOffice, KenRef, restore_health),
    {ok, _} = district:take_action(AndrzejsOffice, AndrzejRef, sneak),
    {ok, _} = district:take_action(Cafeen, PrebenRef, need_to_pee),

    % That morning, Bob thought he could sneak into Lille UP1 before Andrzej,
    % but he was already too late
    % =====| Following two lines changed in ver. 1.0.1 | =====
    % {ok, _} = district:take_action(Canteen, hd(StudentRefs), have_courage),
    % {ok, _} = district:take_action(CoffeeMachine, KenRef, surprise_attack),
    Student = hd(StudentRefs),
    {ok, _} = district:take_action(Canteen, Student, have_courage),
    {ok, _} = district:take_action(LilleUP1, Student, rejuvenate),
    {KensOffice, AndrzejsOffice, Canteen}.
