:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

:- dynamic(card/4).

% TODO basic lands are builtin rn, too annoying to parse from API 
card("Plains", "", land, "{T}: add {W}.").
card("Island", "", land, "{T}: add {U}.").
card("Swamp", "", land, "{T}: add {B}.").
card("Mountain", "", land, "{T}: add {R}.").
card("Forest", "", land, "{T}: add {G}.").

card("Lava Spike", "{R}", sorcery, "Lava Spike deals 3 damage to target player or planeswalker.").
%card("Bump in the Night", "{B}", sorcery, "Target opponent loses 3 life"). % flashback 5R
card("Mind Sculpt", "{1}{U}", sorcery, "Target opponent mills seven cards.").
%card("Scalding Tongs", "{2}", artifact, "At the beginning of your upkeep, if you have three or fewer cards in hand, Scalding Tongs deals 1 damage to target opponent or planeswalker."). 

/*
test(Set) :-
    %get_card("Lava Spike", _),
    %get_card("Mind Sculpt", _),
    %get_card("Bump in the Night", _),
    %get_card("Scalding Tongs", _).
    format(string(URL), 'https://api.magicthegathering.io/v1/cards?set=~w', [Set]),
    http_get(URL, Data, [application/json, json_object(dict)]),
    foreach(member(Card, Data.cards), assert_card_from_json(Card)),
    findall(Name, card(Name,_,_,_), Names),
    foreach(member(Name, Names), (
            card(Name, Cost, Type, Text),
            string_replace(Text, Name, "CARDNAME", Replaced),
            string_lower(Replaced, Lowered),
            string_replace(Lowered, ".", " .", Dot),
            string_replace(Dot, ",", " ,", Comma),
            string_replace(Comma, ":", " :", Colon),
            split_string(Colon, " ", " ", Split),
        (
            parse_rules(Split, Rules),
            format('Succes: ~w ~w ~w ~w\n', [Name, Cost, Type, Rules])
        )
        ;
        (
            format('Fail: ~w ~w\n', [Name, Text])
        )
    )).
*/

get_card_details(Name, CMC, Type, Rules) :-
    (
        card_details(Name, CMC, Type, Rules)
    ;
        get_card(Name),
        card_details(Name, CMC, Type, Rules)
    ).

% assumptions: only one hit, name has spaces replaced with %20
get_card(Name) :-
    string_replace(Name, " ", "%20", URLSafe),
    format(string(URL), 'https://api.magicthegathering.io/v1/cards?name="~w"', [URLSafe]),
    http_get(URL, Data, [application/json, json_object(dict)]),
    Data.cards = [CardJSON|_],
    assert_card_from_json(CardJSON).

assert_card_from_json(CardJSON) :-
    type(CardJSON.type, Type),
    (
        Type \= "land", Type \= "basic",
        Card = card(CardJSON.name, CardJSON.manaCost, CardJSON.type, CardJSON.text)
    ;
        (Type = "land"; Type = "basic"),
        Card = card(CardJSON.name, 0, CardJSON.type, CardJSON.text)
    ),
    assertz(Card).

card_details(Name, CMC, Type, Rules) :-
    card(Name, Cost, FullType, Text),
    cmc(Cost, CMC),
    type(FullType, Type),
    rules(Name, Text, Rules).

type(Raw, Type) :-
    string_lower(Raw, Lowered),
    split_string(Lowered, " ", "", Split),
    Split = [TypeString|_],
    atom_string(Type, TypeString).

% from api, manacost is given as {2}{U} for example
cmc(0, 0).
cmc(Cost, CMC) :-
    string(Cost),
    split_string(Cost, "}", "{", Split),
    cmc(CMC, Split, []).

cmc(0) --> [""].
cmc(X) --> [NS], cmc(M), {number_string(N, NS), integer(N), X #= N + M}.
cmc(Y) --> [M], cmc(X), {mana_symbol(M), Y #= X+1}.

mana_symbol(String) :-
    memberchk(String, ["W","B","U","R","G","{w}","{b}","{u}","{r}","{g}"]).

string_replace(String, Old, New, Replaced) :-
    (
        bagof([Before, Length, After], sub_string(String, Before, Length, After, Old), Bag),
        foldl([A,B,C]>>(
            A = [Before, Length, After],
            B = PrevS-PrevN,
            Len #= Before - PrevN,
            sub_string(String, PrevN, Len, _, SubString),
            NextN #= Before + Length,
            concat(PrevS, SubString, TempS),
            concat(TempS, New, NextS),
            C = NextS-NextN
        ), Bag, ""-0, AlmostS-N),
        sub_string(String, N, _, 0, LastS),
        concat(AlmostS, LastS, Replaced)
    ;
        not(bagof([Before, Length, After], sub_string(String, Before, Length, After, Old), _)),
        String = Replaced
    ).

rules(Name, Text, Rules) :-
    string_replace(Text, Name, "CARDNAME", Replaced),
    string_lower(Replaced, Lowered),
    string_replace(Lowered, ". ", ".\n", DotSlashN),
    string_replace(DotSlashN, ".", " .", Dot),
    string_replace(Dot, ",", " ,", Comma),
    string_replace(Comma, ":", " :", Colon),
    string_replace(Colon, "\n", "@", Temp),
    split_string(Temp, "@", "", SplitNewline),
    maplist([X,Y]>>(split_string(X, " ", " ", Split), parse_rules(Split, Y)), SplitNewline, Rules).

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
parse_cost(colorless(1)) --> ["{1}"].
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
parse_effect(add_mana(X)) --> ["add"], [X], {mana_symbol(X)}.
parse_effect(add_mana(any)) --> ["add", "one", "mana", "of", "any", "color"].
parse_effect(add_mana_choice(X, Y)) --> ["add"], [X], ["or"], [Y], {mana_symbol(X), mana_symbol(Y)}.
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
