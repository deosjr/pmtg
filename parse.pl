rules(Name, Text, Rules) :-
    string_replace(Text, Name, "CARDNAME", Replaced),
    string_lower(Replaced, Lowered),
    string_replace(Lowered, ".", " .", Dot),
    string_replace(Dot, ",", " ,", Comma),
    string_replace(Comma, ":", " :", Colon),
    string_replace(Colon, "\n", "@", Temp),
    split_string(Temp, "@", "", SplitNewline),
    (
        SplitNewline = [""],
        Rules = []
    ;
        SplitNewline \= [""],
        maplist([X,Y]>>(split_string(X, " ", " ", Split), parse_rules(Split, Y)), SplitNewline, Rules)
    ),!.

parse_rules(Split, Rules) :-
    parse_rules(Rules, Split, []).

% theres order to rules: alternative cost, ability keywords, then other effects
% currently this is ignored: each rule split by newline is parsed independently
parse_rules(alternate_cost(C)) --> parse_alternate_cost(C).
parse_rules(alternate_cost(conditional(Condition,Cost))) --> ["if"], parse_condition(Condition), [","], parse_alternate_cost(Cost).
parse_rules(keywords(A)) --> parse_ability_keywords(A).
parse_rules(R) --> parse_effect(R), ["."].
parse_rules(activated_ability(Cost, Effect)) -->
    parse_activation_cost(Cost), [":"], parse_effect(Effect), ["."].
parse_rules(triggered_ability(Trigger, Effect)) --> parse_trigger(Trigger), [","], parse_effect(Effect), ["."].

% triggers
parse_trigger(phase_trigger(Phase)) --> ["at"], parse_phase(Phase),
    {phase_steps(_, Phases), memberchk(Phase, Phases)}.
parse_trigger(etb) --> ["when", "cardname", "enters", "the", "battlefield"].
parse_trigger(dies) --> ["when", "cardname", "dies"].

parse_phase(upkeep) --> ["the", "beginning", "of", "your", "upkeep"].

parse_alternate_cost(C) --> ["you", "may"], parse_costs(C), ["rather", "than", "pay", "this", "spell's", "mana", "cost", "."].

% abilities
parse_ability_keywords([A]) --> parse_ability_keyword(A).
parse_ability_keywords([A|AA]) --> parse_ability_keyword(A), [","], parse_ability_keywords(AA).
parse_ability_keyword(flying) --> ["flying"].
parse_ability_keyword(vigilance) --> ["vigilance"].
parse_ability_keyword(lifelink) --> ["lifelink"].
parse_ability_keyword(haste) --> ["haste"].
parse_ability_keyword(trample) --> ["trample"].

parse_activation_cost([X|Y]) --> parse_cost(X), [","], parse_activation_cost(Y).
parse_activation_cost([X]) --> parse_cost(X).
parse_costs([X|[Y]]) --> parse_cost(X), ["and"], parse_costs(Y).
parse_costs(X) --> parse_cost(X).
parse_cost(tap) --> ["{t}"].
parse_cost(MC) --> [X], {mana_cost(X, MC)}.
parse_cost(MC) --> ["pay"], [X], {mana_cost(X, MC)}.
parse_cost(discard(1)) --> ["discard", "a", "card"].
parse_cost(pay_life(N)) --> ["pay"], parse_number(N), ["life"].
parse_cost(sacrifice_this) --> ["sacrifice", "cardname"].
parse_cost(exile_from_hand(blue_card)) --> ["exile", "a", "blue", "card", "from", "your", "hand"].

% conditions
parse_condition(cards_in_hand(C)) --> ["you", "have"], parse_comparison(C), ["cards", "in", "hand"].
parse_condition(control(swamp)) --> ["you", "control", "a", "swamp"].
parse_comparison(or_less(N)) --> parse_number(N), ["or", "fewer"].

% effects
parse_effect(conditional(Condition, Effect)) --> ["if"], parse_condition(Condition), [","], parse_effect(Effect).
parse_effect(may(Effect)) --> ["you", "may"], parse_effect(Effect).
parse_effect(damage(Target, N)) --> (["cardname"];["it"]), ["deals"], parse_number(N), ["damage", "to"], parse_target(Target).
parse_effect(gain_life(Target, N)) --> parse_target(Target), (["gain"];["gains"]), parse_number(N), ["life"].
parse_effect(lose_life(Target, N)) --> parse_target(Target), (["lose"];["loses"]), parse_number(N), ["life"].
parse_effect(mill(Target, N)) --> parse_target(Target), (["mill"];["mills"]), parse_number(N), ["cards"].
parse_effect(add_mana(MC)) --> ["add", X], {mana_cost(X, MC)}.
parse_effect(add_mana(any, N)) --> ["add"], parse_number(N), ["mana", "of", "any", "color"].
parse_effect(add_mana(any_same, N)) --> ["add"], parse_number(N), ["mana", "of", "any", "one", "color"].
parse_effect(add_mana(or(MX, MY))) --> ["add", X, "or", Y], {mana_cost(X, MX), mana_cost(Y, MY)}.
parse_effect(draw(you, 1)) --> ["draw", "a", "card"].
parse_effect(draw(you, N)) --> ["draw"], parse_number(N), ["cards"].
parse_effect(draw(Target, 1)) --> parse_target(Target), ["draws", "a", "card"].
parse_effect(draw(Target, N)) --> parse_target(Target), ["draws"], parse_number(N), ["cards"].
parse_effect(discard(you, 1)) --> ["discard", "a", "card"].
parse_effect(discard(you, N)) --> ["discard"], parse_number(N), ["cards"].
parse_effect(discard(Target, 1)) --> parse_target(Target), ["discards", "a", "card"].
parse_effect(discard(Target, N)) --> parse_target(Target), ["discards"], parse_number(N), ["cards"].
parse_effect(destroy(Target)) --> ["destroy"], parse_target(Target).
parse_effect(destroy_no_regen(Target)) --> ["destroy"], parse_target(Target), [".", _, "can't", "be", "regenerated"].
parse_effect(counter(Target)) --> ["counter"], parse_target(Target).

% replacement effects
parse_effect(enters_tapped) --> ["cardname", "enters", "the", "battlefield", "tapped"].

% targets
parse_target(you) --> ["you"].
parse_target(any) --> ["any", "target"].
parse_target(Target) --> ["target"], parse_target_mod(Target).
parse_target(all(Target)) --> ["all"], parse_target_mod(Target).
parse_target(all(Target)) --> ["each"], parse_target_mod(Target).

parse_target_mod(Target) --> parse_targetable(Target).
parse_target_mod(conditional(Target, Cond)) --> parse_target_condition(Cond), parse_targetable(Target).
parse_target_mod(conditional(Target, Cond)) --> parse_targetable(Target), parse_target_condition(Cond).
parse_target_mod(or(X, Y)) --> parse_targetable(X), ["or"], parse_targetable(Y).

parse_targetable(player) --> ["player"].
parse_targetable(opponent) --> ["opponent"].
parse_targetable(planeswalker) --> ["planeswalker"].
parse_targetable(creature) --> ["creature"];["creatures"].
parse_targetable(land) --> ["land"].
parse_targetable(artifact) --> ["artifact"].
parse_targetable(enchantment) --> ["enchantment"].
parse_targetable(spell) --> ["spell"].
parse_targetable(permanent) --> ["permanent"].

parse_target_condition(is_tapped) --> ["tapped"].
parse_target_condition(has_flying) --> ["with", "flying"].
parse_target_condition(nonbasic) --> ["nonbasic"].
parse_target_condition(nonblack) --> ["nonblack"].

% numbers
parse_number(N) --> [NS], {number_string(N, NS), integer(N)}.
parse_number(1) --> ["one"].
parse_number(2) --> ["two"].
parse_number(3) --> ["three"].
parse_number(4) --> ["four"].
parse_number(5) --> ["five"].
parse_number(6) --> ["six"].
parse_number(7) --> ["seven"].

parse_mana_cost([0,0,0,0,0,0]) --> [].
parse_mana_cost([C, NW, U, B, R, G]) --> ["w"], parse_mana_cost([C, W, U, B, R, G]), {NW #= W+1}.
parse_mana_cost([C, W, NU, B, R, G]) --> ["u"], parse_mana_cost([C, W, U, B, R, G]), {NU #= U+1}.
parse_mana_cost([C, W, U, NB, R, G]) --> ["b"], parse_mana_cost([C, W, U, B, R, G]), {NB #= B+1}.
parse_mana_cost([C, W, U, B, NR, G]) --> ["r"], parse_mana_cost([C, W, U, B, R, G]), {NR #= R+1}.
parse_mana_cost([C, W, U, B, R, NG]) --> ["g"], parse_mana_cost([C, W, U, B, R, G]), {NG #= G+1}.
parse_mana_cost([NC, W, U, B, R, G]) --> ["c"], parse_mana_cost([C, W, U, B, R, G]), {NC #= C+1}.
parse_mana_cost([NC, W, U, B, R, G]) --> [NS], parse_mana_cost([C, W, U, B, R, G]), {number_string(N, NS), integer(N), NC #= C+N}.

:- begin_tests(parse_card_rules).

test(lava_spike) :-
    Name = "Lava Spike",
    Text = "Lava Spike deals 3 damage to target player or planeswalker.",
    rules(Name, Text, Rules),
    assertion(Rules = [damage(or(player,planeswalker), 3)]).

test(mind_sculpt) :-
    Name = "Mind Sculpt",
    Text = "Target opponent mills seven cards.",
    rules(Name, Text, Rules),
    assertion(Rules = [mill(opponent, 7)]).

test(basic_land) :-
    Name = "Forest",
    Text = "{T}: Add {G}.",
    rules(Name, Text, Rules),
    assertion(Rules = [activated_ability([tap], add_mana([0,0,0,0,0,1]))]).

test(vanilla, [true]) :-
    Name = "Grizzly Bears",
    Text = "",
    rules(Name, Text, []).

test(french_vanilla) :-
    Name = "Colossal Dreadmaw",
    Text = "Trample",
    rules(Name, Text, Rules),
    assertion(Rules = [keywords([trample])]).

test(ability_plus_rules) :-
    Name = "Arborback Stomper",
    Text = "Trample\nWhen Arborback Stomper enters the battlefield, you gain 5 life.",
    rules(Name, Text, Rules),
    assertion(Rules = [keywords([trample]),triggered_ability(etb,gain_life(you,5))]).

test(three_abilities) :-
    Name = "Mantis Rider",
    Text = "Flying, vigilance, haste",
    rules(Name, Text, Rules),
    assertion(Rules = [keywords([flying,vigilance,haste])]).

test(meteorite) :-
    Name = "Meteorite",
    Text = "When Meteorite enters the battlefield, it deals 2 damage to any target.\n{T}: Add one mana of any color.",
    rules(Name, Text, Rules),
    assertion(Rules = [
        triggered_ability(etb, damage(any, 2)), 
        activated_ability([tap], add_mana(any,1))
    ]).

test(thrashing_brontodon) :-
    Name = "Thrashing Brontodon",
    Text = "{1}, Sacrifice Thrashing Brontodon: Destroy target artifact or enchantment.",
    rules(Name, Text, Rules),
    assertion(Rules = [activated_ability([[1,0,0,0,0,0], sacrifice_this], destroy(or(artifact, enchantment)))]).

test(ancestral_recall) :-
    Name = "Ancestral Recall",
    Text = "Target player draws 3 cards.",
    rules(Name, Text, Rules),
    assertion(Rules = [draw(player,3)]).

test(black_lotus) :-
    Name = "Black Lotus",
    Text = "{T}, Sacrifice Black Lotus: Add 3 mana of any one color.",
    rules(Name, Text, Rules),
    assertion(Rules = [activated_ability([tap, sacrifice_this], add_mana(any_same,3))]).

test(wasteland) :-
    Name = "Wasteland",
    Text = "{T}: Add {C}.\n{T}, Sacrifice Wasteland: Destroy target nonbasic land.",
    rules(Name, Text, Rules),
    assertion(Rules = [
        activated_ability([tap], add_mana([1,0,0,0,0,0])),
        activated_ability([tap, sacrifice_this], destroy(conditional(land,nonbasic)))
    ]).

test(counterspell) :-
    Name = "Counterspell",
    Text = "Counter target spell.",
    rules(Name, Text, Rules),
    assertion(Rules = [counter(spell)]).

test(dualland) :-
    Name = "Tundra",
    Text = "{T}: Add {W} or {U}.",
    rules(Name, Text, Rules),
    assertion(Rules = [activated_ability([tap],add_mana(or([0,1,0,0,0,0],[0,0,1,0,0,0])))]).

test(darkritual) :-
    Name = "Dark Ritual",
    Text = "Add {B}{B}{B}.",
    rules(Name, Text, Rules),
    assertion(Rules = [add_mana([0,0,0,3,0,0])]).

test(destroy_all) :-
    Name = "Day of Judgment",
    Text = "Destroy all creatures.",
    rules(Name, Text, Rules),
    assertion(Rules = [destroy(all(creature))]).

test(destroy_all_conditional) :-
    Name = "Whirlwind",
    Text = "Destroy all creatures with flying.",
    rules(Name, Text, Rules),
    assertion(Rules = [destroy(all(conditional(creature,has_flying)))]).

test(damage_all) :-
    Name = "Pyroclasm",
    Text = "Pyroclasm deals 2 damage to each creature.",
    rules(Name, Text, Rules),
    assertion(Rules = [damage(all(creature),2)]).

test(destroy_no_regen) :-
    Name = "Oxidize",
    Text = "Destroy target artifact. It can't be regenerated.",
    rules(Name, Text, Rules),
    assertion(Rules = [destroy_no_regen(artifact)]).

test(wrath_of_god) :-
    Name = "Wrath of God",
    Text = "Destroy all creatures. They can't be regenerated.",
    rules(Name, Text, Rules),
    assertion(Rules = [destroy_no_regen(all(creature))]).

test(doom_blade) :-
    Name = "Doom Blade",
    Text = "Destroy target nonblack creature.",
    rules(Name, Text, Rules),
    assertion(Rules = [destroy(conditional(creature,nonblack))]).

test(alternate_cost) :-
    Name = "Bringer of the Blue Dawn",
    Text = "You may pay {W}{U}{B}{R}{G} rather than pay this spell's mana cost.\nTrample\nAt the beginning of your upkeep, you may draw two cards.",
    rules(Name, Text, Rules),
    assertion(Rules = [alternate_cost([0,1,1,1,1,1]),keywords([trample]),triggered_ability(phase_trigger(upkeep),may(draw(you,2)))]).

test(snuff_out) :-
    Name = "Snuff Out",
    Text = "If you control a Swamp, you may pay 4 life rather than pay this spell's mana cost.\nDestroy target nonblack creature. It can't be regenerated.",
    rules(Name, Text, Rules),
    assertion(Rules = [alternate_cost(conditional(control(swamp),pay_life(4))),destroy_no_regen(conditional(creature,nonblack))]).

test(force_of_will) :-
    Name = "Force of Will",
    Text = "You may pay 1 life and exile a blue card from your hand rather than pay this spell's mana cost.\nCounter target spell.",
    rules(Name, Text, Rules),
    assertion(Rules = [alternate_cost([pay_life(1),exile_from_hand(blue_card)]),counter(spell)]).

:- end_tests(parse_card_rules).
