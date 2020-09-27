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
    (
        get_dict(text, CardJSON, Text)
    ;
        Text = ""
    ),
    type(CardJSON.type, Type),
    (
        Type \= land, Type \= basic,
        Card = card(CardJSON.name, CardJSON.manaCost, CardJSON.type, Text)
    ;
        (Type = land; Type = basic),
        Card = card(CardJSON.name, 0, CardJSON.type, Text)
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
cmc([C, W, U, B, R, G], CMC) :-
    CMC #= C+W+U+B+R+G.

% cost: colorless, w, u, b, r, g
mana_cost(String, Cost) :-
    split_string(String, "{", "", Split),
    maplist([X,Y]>>(string_replace(X, "}", "", Y)), Split, [""|Mapped]),
    parse_mana_cost(Cost, Mapped, []).

string_replace(String, Old, New, Replaced) :-
    (
        bagof([Before, Length, After], sub_string(String, Before, Length, After, Old), Bag),
        foldl([A,B,C]>>(
            A = [Before, Length, After],
            B = PrevS-PrevN,
            Len #= Before - PrevN,
            sub_string(String, PrevN, Len, _, SubString),
            NextN #= Before + Length,
            string_concat(PrevS, SubString, TempS),
            string_concat(TempS, New, NextS),
            C = NextS-NextN
        ), Bag, ""-0, AlmostS-N),
        sub_string(String, N, _, 0, LastS),
        string_concat(AlmostS, LastS, Replaced)
    ;
        not(bagof([Before, Length, After], sub_string(String, Before, Length, After, Old), _)),
        String = Replaced
    ).

