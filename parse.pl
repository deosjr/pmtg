:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

rules(Name, Text, Rules) :-
    string_replace(Text, Name, "CARDNAME", Replaced),
    string_lower(Replaced, Lowered),
    string_codes(Lowered, Codes),
    phrase(parse_rules(Rules), Codes),!.

parse_rules([]) --> [].
parse_rules([R|T]) --> parse_rule(R), blanks_to_nl, parse_rules(T).

% theres order to rules: alternative cost, ability keywords, then other effects
% currently this is ignored: each rule split by newline is parsed independently
parse_rule(alternate_cost(C)) --> parse_alternate_cost(C).
parse_rule(alternate_cost(conditional(Condition,Cost))) --> "if ", parse_condition(Condition), ", ", parse_alternate_cost(Cost).
parse_rule(keywords(A)) --> parse_ability_keywords(A).
parse_rule(E) --> parse_complex_keyword(E).
parse_rule(E) --> parse_effects(E), ".", opt_rules_explanation.
parse_rule(activated_ability(Cost, Effect)) --> parse_activation_cost(Cost), ": ", parse_effects(Effect), ".", opt_rules_explanation.
parse_rule(activated_ability(conditional(your_turn_only,Cost), Effect)) --> parse_activation_cost(Cost), ": ", parse_effects(Effect), ". activate only during your turn.".
parse_rule(activated_ability(conditional(spend(C),Cost), Effect)) --> parse_activation_cost(Cost), ": ", parse_effects(Effect), ". spend only ", parse_color(C), " mana on x.".
parse_rule(triggered_ability(Trigger, Effect)) --> parse_trigger(Trigger), ", ", parse_effects(Effect), ".".
parse_rule(triggered_ability(conditional(Condition,Trigger), Effect)) --> parse_trigger(Trigger), ", ", "if ", parse_condition(Condition), ", ", parse_effects(Effect), ".".

% (basic) land reminder text
parse_rule(activated_ability(Cost, Effect)) --> "(", parse_activation_cost(Cost), ": ", parse_effects(Effect), ".)".

% triggers
parse_trigger(phase_trigger(begin(Phase,Player))) --> "at the beginning of ", parse_player(Player), " ", parse_step(Phase).
parse_trigger(etb(T))     --> ("when ";"whenever "), parse_target(T), " enters the battlefield".
parse_trigger(dies(T))    --> ("when ";"whenever "), parse_target(T), " dies".
parse_trigger(attacks(T)) --> "whenever ", parse_target(T), " attacks".
parse_trigger(blocks(T))  --> "whenever ", parse_target(T), " blocks".
% TODO: should this parse into 2 separate triggers, one for 'blocks' and one for 'becomes blocked' ?
parse_trigger(blocks_or_becomes_blocked(T))  --> "whenever ", parse_target(T), " blocks or becomes blocked".
parse_trigger(tapped)     --> "whenever cardname becomes tapped".
parse_trigger(deals_damage(self,T)) --> "whenever cardname deals damage to ", parse_target(T).
parse_trigger(deals_combat_damage(self,T)) --> "whenever cardname deals combat damage to ", parse_target(T).
parse_trigger(casts(Player,Spell)) --> "whenever ", parse_target(Player), (" casts ";" cast "), parse_target(Spell).

parse_player(you)  --> "your".
parse_player(each) --> "each player's".

parse_step(upkeep) --> "upkeep".
parse_step(draw)   --> "draw step".
parse_step(combat) --> "combat step".
parse_step(ending) --> "end step".

parse_alternate_cost(C) --> "you may ", parse_costs(C), " rather than pay this spell's mana cost.".

% abilities
parse_ability_keywords([A])      --> parse_ability_keyword(A), opt_rules_explanation.
parse_ability_keywords([A|AA])   --> parse_ability_keyword(A), ", ", parse_ability_keywords(AA).
parse_ability_keyword(flying)    --> "flying".
parse_ability_keyword(vigilance) --> "vigilance".
parse_ability_keyword(lifelink)  --> "lifelink".
parse_ability_keyword(haste)     --> "haste".
parse_ability_keyword(trample)   --> "trample".
parse_ability_keyword(defender)  --> "defender".
parse_ability_keyword(fear)      --> "fear".
parse_ability_keyword(reach)     --> "reach".
parse_ability_keyword(walk(L))   --> parse_basic(L), "walk".
parse_ability_keyword(first_strike) --> "first strike".
parse_ability_keyword(cant_block)   --> "cardname can't block.".

% keywords that can be replaced with ruletext
parse_complex_keyword(R) --> "bushido ", integer(N), opt_rules_explanation,
    {format(string(S), 'whenever cardname blocks or becomes blocked, it gets +~D/+~D until end of turn.', [N,N]),
    string_codes(S,C), phrase(parse_rule(R), C)}.
parse_complex_keyword(R) --> "prowess", opt_rules_explanation,
    {string_codes("whenever you cast a noncreature spell, cardname gets +1/+1 until end of turn.", C), phrase(parse_rule(R), C)}.

opt_rules_explanation --> ("";(" (", string_without(")", _), ")")).

parse_activation_cost([X|Y])--> parse_cost(X), ", ", parse_activation_cost(Y).
parse_activation_cost([X])  --> parse_cost(X).
parse_costs([X|[Y]])        --> parse_cost(X), " and ", parse_costs(Y).
parse_costs(X)              --> parse_cost(X).
parse_cost(tap)             --> "{t}".
parse_cost(MC)              --> parse_mana_cost(MC).
parse_cost(MC)              --> "pay ", parse_mana_cost(MC).
parse_cost(discard(N))      --> "discard ", parse_number(N), " card", opt_s.
parse_cost(discard(T,N))    --> "discard ", parse_number(N), " ", parse_type(T), " card", opt_s.
parse_cost(pay_life(N))     --> "pay ", parse_number(N), " life".
parse_cost(sacrifice(T))    --> "sacrifice ", parse_target(T).
parse_cost(exile_from_hand(blue_card)) --> "exile a blue card from your hand".

% conditions
parse_condition(cards_in_hand(C)) --> "you have ", parse_comparison(C), " cards in hand".
parse_condition(control(L))       --> "you control a ", parse_basic(L).
parse_condition(life(C))          --> "you have ", parse_comparison(C), " life".
parse_condition(untapped(self))   --> "cardname is untapped".

parse_comparison(or_more(N)) --> parse_number(N), " or greater".
parse_comparison(or_more(N)) --> parse_number(N), " or more".
parse_comparison(or_less(N)) --> parse_number(N), " or less".
parse_comparison(or_less(N)) --> parse_number(N), " or fewer".

% effects
parse_effects(conditional(Condition, Effect)) --> "if ", parse_condition(Condition), ", ", parse_effect(Effect).
parse_effects(conditional(Cost, Effect)) --> "you may ", parse_cost(Cost), ". if you do, ", parse_effect(Effect).
parse_effects(and(E1, E2)) --> parse_effect(E1), ". ", parse_effect(E2).
parse_effects(and(E1, E2)) --> parse_effect(E1), " and ", parse_effect(E2).
parse_effects(and(E1, E2)) --> parse_effect(E1), ", then ", parse_effect(E2).
parse_effects(may(Effect)) --> "you may ", parse_effect(Effect).
parse_effects(E) --> parse_effect(E).
parse_effect(damage(Target, N))     --> ("cardname";"it"), " deals ", parse_number(N), " damage to ", parse_target(Target).
parse_effect(damage([T1,T2], N))    --> ("cardname";"it"), " deals ", parse_number(N), " damage to ", parse_target(T1), " and ", parse_target(T2).
parse_effect(gain_life(Target, N))  --> parse_target(Target), (" gain ";" gains "), parse_number(N), " life".
parse_effect(lose_life(Target, N))  --> parse_target(Target), (" lose ";" loses "), parse_number(N), " life".
parse_effect(add_mana(MC))          --> "add ", parse_mana_cost(MC).
parse_effect(add_mana(any, N))      --> "add ", parse_number(N), " mana of any color".
parse_effect(add_mana(any_same, N)) --> "add ", parse_number(N), " mana of any one color".
parse_effect(add_mana(or(X, Y)))    --> "add ", parse_mana_cost(X), " or ", parse_mana_cost(Y).
parse_effect(draw(you, N))      --> "draw ", parse_number(N), " card", opt_s.
parse_effect(draw(Target, N))   --> parse_target(Target), " draws ", parse_number(N), " card", opt_s.
parse_effect(discard(you, N))   --> "discard ", parse_number(N), " card", opt_s.
parse_effect(discard(Target, N))--> parse_target(Target), " discards ", parse_number(N), " card", opt_s.
parse_effect(destroy(Target))           --> "destroy ", parse_target(Target).
parse_effect(destroy_no_regen(Target))  --> "destroy ", parse_target(Target), ". ", ("it";"they"), " can't be regenerated".
parse_effect(counter(Target)) --> "counter ", parse_target(Target).
parse_effect(mill(Target, N)) --> parse_target(Target), (" mill ";" mills "), parse_number(N), (" card";" cards").
parse_effect(stat_change(Target, P/T)) --> parse_target(Target), (" get ";" gets "), integer(P), "/", integer(T), " until end of turn".
parse_effect(ability_gain(Target, A))  --> parse_target(Target), " gains ", parse_ability_keyword(A), " until end of turn".
parse_effect(tap(Target))   --> "tap ", parse_target(Target).
parse_effect(untap(Target)) --> "untap ", parse_target(Target).
parse_effect(return(Target,battlefield,hand)) --> "return ", parse_target(Target), " to ", ("its owner's hand";"their owners' hands").
parse_effect(return(Target,graveyard,hand))   --> "return ", parse_target(Target), " card from your graveyard to your hand".
parse_effect(regenerate(Target))  --> "regenerate ", parse_target(Target).
parse_effect(sacrifice(Target,Sacrifice)) --> parse_target(Target), " sacrifice", opt_s, " ", parse_target(Sacrifice).
parse_effect(search(Target,play))         --> "search your library for ", parse_target(Target), " card, put that card onto the battlefield, then shuffle".
parse_effect(search(Target,play_tapped))  --> "search your library for ", parse_target(Target), " card, put that card onto the battlefield tapped, then shuffle".

% replacement effects
parse_effect(enters_tapped) --> "cardname enters the battlefield tapped".

% targets
parse_target(you)           --> "you".
parse_target(any)           --> "any target".
parse_target(self)          --> "cardname".
parse_target(ana)           --> "it";("that ",parse_type(_)). % anaphoric reference, to be resolved by the interpreter
parse_target(controller)    --> "its controller".  % anaphoric reference, to be resolved by the interpreter
parse_target(Target)        --> "target ", parse_target_mod(Target).
parse_target(all(Target))   --> "all ", parse_target_mod(Target).
parse_target(all(Target))   --> "each ", parse_target_mod(Target).
parse_target(any(Target))   --> ("a ";"an "), parse_target_mod(Target). % TODO: not targeting! perhaps this should be 'entities' or whatever
parse_target(multiple(Target, N)) --> parse_number(N), " ", parse_target_mod(Target).
parse_target(all(creature, Cond)) --> "creatures ", parse_target_condition(Cond).

parse_target_mod(Target) --> parse_targetable(Target).
parse_target_mod(conditional(Target, Cond)) --> parse_target_condition(Cond), " ", parse_targetable(Target).
parse_target_mod(conditional(Target, Cond)) --> parse_targetable(Target), " ", parse_target_condition(Cond).
parse_target_mod(conditional(Target, [Cond1,Cond2])) --> parse_target_condition(Cond1), " ", parse_targetable(Target), " ", parse_target_condition(Cond2).
parse_target_mod(or(X, Y))    --> parse_targetable(X), " or ", parse_targetable(Y).
parse_target_mod(or(X, Y, Z)) --> parse_targetable(X), ", ", parse_targetable(Y), ", or ", parse_targetable(Z).
parse_target_mod(and(X, Y))   --> parse_targetable(X), " and ", parse_targetable(Y).

parse_targetable(T) --> parse_type(T).
parse_targetable([X,Y]) --> parse_type(X), " ", parse_type(Y).

parse_type(player)        --> "player", opt_s.
parse_type(opponent)      --> "opponent", opt_s.
parse_type(planeswalker)  --> "planeswalker", opt_s.
parse_type(creature)      --> "creature", opt_s.
parse_type(land)          --> "land", opt_s.
parse_type(artifact)      --> "artifact", opt_s.
parse_type(enchantment)   --> "enchantment", opt_s.
parse_type(permanent)     --> "permanent", opt_s.
parse_type(spell)         --> "spell", opt_s.
parse_type(L)             --> parse_basic(L).

parse_basic(plains)   --> "plains", opt_s.
parse_basic(island)   --> "island", opt_s.
parse_basic(swamp)    --> "swamp", opt_s.
parse_basic(mountain) --> "mountain", opt_s.
parse_basic(forest)   --> "forest", opt_s.

parse_target_condition(power(C))    --> "with power ", parse_comparison(C).
parse_target_condition(toughness(C))--> "with toughness ", parse_comparison(C).
parse_target_condition(is_tapped)   --> "tapped".
parse_target_condition(has_flying)  --> "with flying".
parse_target_condition(no_flying)   --> "without flying".
parse_target_condition(basic)       --> "basic".
parse_target_condition(nonbasic)    --> "nonbasic".
parse_target_condition(C)           --> parse_color(C).
parse_target_condition(not(C))      --> "non", parse_color(C).
parse_target_condition(not(T))      --> "non", parse_type(T).
parse_target_condition(attacking)   --> "attacking".
parse_target_condition(blocking)    --> "blocking".
parse_target_condition(attacking_or_blocking) --> "attacking or blocking".
parse_target_condition(controlled_by(you)) --> "you control".

parse_color(white) --> "white".
parse_color(blue)  --> "blue".
parse_color(black) --> "black".
parse_color(red)   --> "red".
parse_color(green) --> "green".

% numbers
parse_number(N) --> integer(N).
parse_number(1) --> "one";"a";"an additional".
parse_number(2) --> "two".
parse_number(3) --> "three".
parse_number(4) --> "four".
parse_number(5) --> "five".
parse_number(6) --> "six".
parse_number(7) --> "seven".
parse_number(x) --> "x".

parse_mana_cost([0,0,0,0,0,0]) --> [].
parse_mana_cost([C, NW, U, B, R, G]) --> "{w}", parse_mana_cost([C, W, U, B, R, G]), {NW #= W+1}.
parse_mana_cost([C, W, NU, B, R, G]) --> "{u}", parse_mana_cost([C, W, U, B, R, G]), {NU #= U+1}.
parse_mana_cost([C, W, U, NB, R, G]) --> "{b}", parse_mana_cost([C, W, U, B, R, G]), {NB #= B+1}.
parse_mana_cost([C, W, U, B, NR, G]) --> "{r}", parse_mana_cost([C, W, U, B, R, G]), {NR #= R+1}.
parse_mana_cost([C, W, U, B, R, NG]) --> "{g}", parse_mana_cost([C, W, U, B, R, G]), {NG #= G+1}.
parse_mana_cost([NC, W, U, B, R, G]) --> "{c}", parse_mana_cost([C, W, U, B, R, G]), {NC #= C+1}.
parse_mana_cost([NC, W, U, B, R, G]) --> "{",integer(N),"}", parse_mana_cost([C, W, U, B, R, G]), {NC #= C+N}.
parse_mana_cost([x, W, U, B, R, G])  --> "{x}", parse_mana_cost([0, W, U, B, R, G]).

opt_s --> "";"s". % optional 's' when singular/plural doesnt matter

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
    assertion(Rules = [keywords([trample]),triggered_ability(etb(self),gain_life(you,5))]).

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
        triggered_ability(etb(self), damage(any, 2)), 
        activated_ability([tap], add_mana(any,1))
    ]).

test(thrashing_brontodon) :-
    Name = "Thrashing Brontodon",
    Text = "{1}, Sacrifice Thrashing Brontodon: Destroy target artifact or enchantment.",
    rules(Name, Text, Rules),
    assertion(Rules = [activated_ability([[1,0,0,0,0,0], sacrifice(self)], destroy(or(artifact, enchantment)))]).

test(ancestral_recall) :-
    Name = "Ancestral Recall",
    Text = "Target player draws 3 cards.",
    rules(Name, Text, Rules),
    assertion(Rules = [draw(player,3)]).

test(black_lotus) :-
    Name = "Black Lotus",
    Text = "{T}, Sacrifice Black Lotus: Add 3 mana of any one color.",
    rules(Name, Text, Rules),
    assertion(Rules = [activated_ability([tap, sacrifice(self)], add_mana(any_same,3))]).

test(wasteland) :-
    Name = "Wasteland",
    Text = "{T}: Add {C}.\n{T}, Sacrifice Wasteland: Destroy target nonbasic land.",
    rules(Name, Text, Rules),
    assertion(Rules = [
        activated_ability([tap], add_mana([1,0,0,0,0,0])),
        activated_ability([tap, sacrifice(self)], destroy(conditional(land,nonbasic)))
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
    assertion(Rules = [destroy(conditional(creature,not(black)))]).

test(alternate_cost) :-
    Name = "Bringer of the Blue Dawn",
    Text = "You may pay {W}{U}{B}{R}{G} rather than pay this spell's mana cost.\nTrample\nAt the beginning of your upkeep, you may draw two cards.",
    rules(Name, Text, Rules),
    assertion(Rules = [alternate_cost([0,1,1,1,1,1]),keywords([trample]),triggered_ability(phase_trigger(begin(upkeep,you)),may(draw(you,2)))]).

test(snuff_out) :-
    Name = "Snuff Out",
    Text = "If you control a Swamp, you may pay 4 life rather than pay this spell's mana cost.\nDestroy target nonblack creature. It can't be regenerated.",
    rules(Name, Text, Rules),
    assertion(Rules = [alternate_cost(conditional(control(swamp),pay_life(4))),destroy_no_regen(conditional(creature,not(black)))]).

test(force_of_will) :-
    Name = "Force of Will",
    Text = "You may pay 1 life and exile a blue card from your hand rather than pay this spell's mana cost.\nCounter target spell.",
    rules(Name, Text, Rules),
    assertion(Rules = [alternate_cost([pay_life(1),exile_from_hand(blue_card)]),counter(spell)]).

test(anaphor) :-
    Name = "Caltrops",
    Text = "Whenever a creature attacks, Caltrops deals 1 damage to it.",
    rules(Name, Text, Rules),
    % the interpreter of this parse has to figure out that in this pattern, ana refers to the thing triggering
    assertion(Rules = [triggered_ability(attacks(any(creature)),damage(ana,1))]).
    % which will be a little harder for cases where the anaphor is not as straightforward, i.e. Howling Mine:
    % [triggered_ability(phase_trigger(begin(draw,each)),conditional(untapped(self),draw(ana,1)))]
    % Here the ana refers to the player 'each', _not_ to the howling mine 'self' (first possible backreference)

test(conditional_upkeep_trigger) :-
    Name = "Convalescence",
    Text = "At the beginning of your upkeep, if you have 10 or less life, you gain 1 life.",
    rules(Name, Text, Rules),
    assertion(Rules = [triggered_ability(phase_trigger(begin(upkeep,you)),conditional(life(or_less(10)),gain_life(you,1)))]).

test(combat_damage_player) :-
    Name = "Blazing Specter",
    Text = "Whenever Blazing Specter deals combat damage to a player, that player discards a card.",
    rules(Name, Text, Rules),
    assertion(Rules = [triggered_ability(deals_combat_damage(self,any(player)),discard(ana,1))]).

test(combat_damage_creature) :-
    Name = "Dripping Dead",
    Text = "Dripping Dead can't block.\nWhenever Dripping Dead deals combat damage to a creature, destroy that creature. It can't be regenerated.",
    rules(Name, Text, Rules),
    assertion(Rules = [keywords([cant_block]),triggered_ability(deals_combat_damage(self,any(creature)),destroy_no_regen(ana))]).

test(blocks_becomes_blocked) :-
    Name = "Goblin Elite Infantry",
    Text = "Whenever Goblin Elite Infantry blocks or becomes blocked, it gets -1/-1 until end of turn.",
    rules(Name, Text, Rules),
    assertion(Rules = [triggered_ability(blocks_or_becomes_blocked(self),stat_change(ana,-1/ -1))]).

test(replacing_keywords) :-
    Name = "Jade Avenger",
    Text = "Bushido 2 (Whenever this creature blocks or becomes blocked, it gets +2/+2 until end of turn.)",
    rules(Name, Text, Rules),
    assertion(Rules = [triggered_ability(blocks_or_becomes_blocked(self),stat_change(ana,2/2))]).

test(monastery_swiftspear) :-
    Name = "Monastery Swiftspear",
    Text = "Haste\nProwess (Whenever you cast a noncreature spell, this creature gets +1/+1 until end of turn.)",
    rules(Name, Text, Rules),
    assertion(Rules = [keywords([haste]),triggered_ability(casts(you,any(conditional(spell,not(creature)))),stat_change(self,1/1))]).

:- end_tests(parse_card_rules).

test_from_json(Filename) :-
    open(Filename, read, Stream),
    phrase_from_stream(parse_test_json(Pass/Total), Stream),
    writeln(Pass/Total).

parse_test_json(0/0) --> eos.
parse_test_json(P/T) --> parse_json_line(Pass), blanks_to_nl, parse_test_json(PP/TT),
    { T #= TT + 1, (Pass -> P #= PP + 1 ; P #= PP) }.
parse_json_line(Pass) --> "{", blanks_to_nl, "  \"name\": \"", string_without("\"", NC), "\",",
    blanks_to_nl, "  \"oracle_text\": \"", string_without("\"", TC), "\"", blanks_to_nl, "}",
    {string_codes(Name, NC), string_codes(TextRaw, TC), string_replace(TextRaw, "\\n", "\n", Text),
    ( rules(Name, Text, Rules) -> Pass=true,writeln([Name, Rules, TextRaw]) ; Pass=false,writeln([fail, Name, TextRaw]) )}.

:- begin_tests(json_7ed).

test(json_7ed) :-
    test_from_json("test.json").

:- end_tests(json_7ed).
