rules(Name, Text, Rules) :-
    string_replace(Text, Name, "CARDNAME", Replaced),
    string_lower(Replaced, Lowered),
    string_replace(Lowered, ". ", ".\n", DotSlashN),
    string_replace(DotSlashN, ".", " .", Dot),
    string_replace(Dot, ",", " ,", Comma),
    string_replace(Comma, ":", " :", Colon),
    string_replace(Colon, "\n", "@", Temp),
    split_string(Temp, "@", "", SplitNewline),
    maplist([X,Y]>>(split_string(X, " ", " ", Split), parse_rules(Split, Y)), SplitNewline, Rules), !.

parse_rules(Split, Rules) :-
    parse_rules(Rules, Split, []).

parse_rules(R) --> parse_effect(R), ["."].
parse_rules([E|[R]]) --> parse_effect(E), ["and"], parse_rules(R).
parse_rules(A) --> parse_ability_keyword(A).
parse_rules([A|[R]]) --> parse_ability_keyword(A), [","], parse_ability_keyword(R).

parse_rules(activated_ability(Cost, Effect)) -->
    parse_activation_cost(Cost), [":"], parse_effect(Effect), ["."].

% triggers
parse_rules(triggered_ability(Trigger, Effect)) --> parse_trigger(Trigger), [","], parse_effect(Effect), ["."].
parse_trigger(phase_trigger(Phase)) --> ["at"], parse_phase(Phase),
    {phase_steps(_, Phases), memberchk(Phase, Phases)}.

parse_trigger(etb) --> ["when", "cardname", "enters", "the", "battlefield"].
parse_trigger(dies) --> ["when", "cardname", "dies"].

parse_phase(upkeep) --> ["the", "beginning", "of", "your", "upkeep"].

% abilities
parse_ability_keyword(flying) --> ["flying"].
parse_ability_keyword(vigilance) --> ["vigilance"].
parse_ability_keyword(lifelink) --> ["lifelink"].
parse_ability_keyword(haste) --> ["haste"].
parse_ability_keyword(trample) --> ["trample"].

parse_activation_cost([X|Y]) --> parse_cost(X), [","], parse_activation_cost(Y).
parse_activation_cost([X]) --> parse_cost(X).
parse_activation_cost([]) --> [].
parse_cost(tap) --> ["{t}"].
parse_cost(discard(1)) --> ["discard", "a", "card"].
parse_cost(pay_life(N)) --> ["pay"], parse_number(N), ["life"].
parse_cost(sacrifice_this) --> ["sacrifice", "cardname"].

% conditions
parse_condition(cards_in_hand(C)) --> ["you", "have"], parse_comparison(C), ["cards", "in", "hand"].
parse_comparison(or_less(N)) --> parse_number(N), ["or", "fewer"].

% effects
parse_effect(conditional(Condition, Effect)) --> ["if"], parse_condition(Condition), [","], parse_effect(Effect).
parse_effect(damage(Target, N)) --> (["cardname"];["it"]), ["deals"], parse_number(N), ["damage", "to"], parse_target(Target).
parse_effect(gain_life(Target, N)) --> parse_target(Target), (["gain"];["gains"]), parse_number(N), ["life"].
parse_effect(lose_life(Target, N)) --> parse_target(Target), (["lose"];["loses"]), parse_number(N), ["life"].
parse_effect(mill(Target, N)) --> parse_target(Target), (["mill"];["mills"]), parse_number(N), ["cards"].
parse_effect(add_mana(MC)) --> ["add", X], {mana_cost(X, MC)}.
parse_effect(add_mana(any)) --> ["add", "one", "mana", "of", "any", "color"].
parse_effect(add_mana_choice(X, Y)) --> ["add", X, "or", Y], {mana_cost(X), mana_cost(Y)}.
parse_effect(draw(you, 1)) --> ["draw", "a", "card"].
parse_effect(draw(you, N)) --> ["draw"], parse_number(N), ["cards"].
parse_effect(draw(Target, 1)) --> parse_target(Target), ["draws", "a", "card"].
parse_effect(draw(Target, N)) --> parse_target(Target), ["draws"], parse_number(N), ["cards"].
parse_effect(discard(you, 1)) --> ["discard", "a", "card"].
parse_effect(discard(you, N)) --> ["discard"], parse_number(N), ["cards"].
parse_effect(discard(Target, 1)) --> parse_target(Target), ["discards", "a", "card"].
parse_effect(discard(Target, N)) --> parse_target(Target), ["discards"], parse_number(N), ["cards"].
parse_effect(destroy(Target)) --> ["destroy"], parse_target(Target).

% replacement effects
parse_effect(enters_tapped) --> ["cardname", "enters", "the", "battlefield", "tapped"].

% targets
parse_target(you) --> ["you"].
parse_target(any) --> ["any", "target"].
parse_target(Target) --> ["target"], parse_targetable(Target).
parse_target(conditional(Target, Cond)) --> ["target"], parse_target_condition(Cond), parse_targetable(Target).
parse_target(conditional(Target, Cond)) --> ["target"], parse_targetable(Target), parse_target_condition(Cond).
parse_target(or(X, Y)) --> ["target"], parse_targetable(X), ["or"], parse_targetable(Y).

parse_targetable(player) --> ["player"].
parse_targetable(opponent) --> ["opponent"].
parse_targetable(planeswalker) --> ["planeswalker"].
parse_targetable(creature) --> ["creature"].
parse_targetable(land) --> ["land"].
parse_targetable(artifact) --> ["artifact"].
parse_targetable(enchantment) --> ["enchantment"].

parse_target_condition(is_tapped) --> ["tapped"].
parse_target_condition(has_flying) --> ["with", "flying"].

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

test(forest) :-
    Name = "Forest",
    Text = "{T}: Add {G}.",
    rules(Name, Text, Rules),
    assertion(Rules = [activated_ability([tap], add_mana([0,0,0,0,0,1]))]).

:- end_tests(parse_card_rules).
