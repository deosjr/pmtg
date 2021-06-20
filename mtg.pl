:- use_module(library(clpfd)).

:- ['phases.pl'].
:- ['cards.pl'].
:- ['parse.pl'].
:- ['actions.pl'].

:- dynamic([life/2, hand/2, deck/2, graveyard/2, board/1]).

new_board :-
    assertz(board([])).

% only 2 player games for now: You vs AI
run :-
    PlayerDeckList = [20-"Mountain", 20-"Lava Spike"],
    AIDeckList = [20-"Island", 20-"Mind Sculpt"],
    new_player(PlayerDeckList, player),
    new_player(AIDeckList, ai),
    new_board,
    % TODO: mulligans, start at begin but skip first draw
    game(1-1, player, precombatmain).

% ActivePlayer is one of 'player' or 'ai' atoms
% Turn is TurnNum-PlayerNum pair. 
% If PlayerNum = NumPlayers, everyone has had a turn and a new turn begins
game(Turn, ActivePlayer, Phase) :-
    print_gamestate(Turn, Phase, ActivePlayer),
    phase_steps(Phase, Steps),
    handle_steps(Steps, ActivePlayer),
    next_phase(Phase, NextPhase),
    (
        % turn rollover
        NextPhase = beginning,
        other_player(ActivePlayer, NewActivePlayer),
        (
            Turn = N-1,
            game(N-2, NewActivePlayer, NextPhase)
        ;
            Turn = N-2,
            NewN #= N+1,
            game(NewN-1, NewActivePlayer, NextPhase)
        )
    ;
        % next phase within a turn
        NextPhase \= beginning,
        game(Turn, ActivePlayer, NextPhase)
    ).

print_gamestate(Turn-_, ActivePlayer, Phase) :-
    tty_clear,
    tty_goto(0, 0),
    format("Turn ~w ~w phase. Active player: ~w\n", [Turn, Phase, ActivePlayer]),
    life(player, PLife),
    hand(player, PHand),
    deck(player, PDeck),
    graveyard(player, PGraveyard),
    length(PDeck, PN),
    format("Player life: ~w\nPlayer hand: ~w\nPlayer decksize: ~w\nPlayer graveyard: ~w\n", [PLife, PHand, PN, PGraveyard]),
    life(ai, AILife),
    hand(ai, AIHand),
    deck(ai, AIDeck),
    graveyard(ai, AIGraveyard),
    length(AIHand, AIH),
    length(AIDeck, AIN),
    format("AI life: ~w\nAI handsize: ~w\nAI decksize: ~w\nAI graveyard: ~w\n", [AILife, AIH, AIN, AIGraveyard]),
    board(Board),
    format("Board: ~w\n", [Board]),
    format("-----------------------------------------------------------------------\n", []).

other_player(player, ai).
other_player(ai, player).

% a deck is a sorted list of cards.
% dont have to be instanced, so its a list of cardnames only
% same for a hand, except it doesnt even have to be sorted
% a decklist is a list of number-cardname pairs where each card
% is included #number times in the deck

% asserts a fresh new player
new_player(Decklist, PlayerName) :-
    forall(member(_-Card, Decklist), (
        card(Card, _, _, _) ;
        (format("card not found: ~w\n", [Card]), fail)
    )),
    decklist_to_deck(Decklist, FullDeck),
    shuffle(FullDeck, ShuffledDeck),
    draw_seven(ShuffledDeck, Deck, Hand),
    assertz(life(PlayerName, 20)),
    assertz(hand(PlayerName, Hand)),
    assertz(deck(PlayerName, Deck)),
    assertz(graveyard(PlayerName, [])).

decklist_to_deck([], []).
decklist_to_deck([N-Card|Decklist], FullDeck) :-
    decklist_to_deck(Decklist, PartialDeck),
    length(Copies, N),
    % TODO: how to cleanly fill a list with copies again?
    maplist({Card}/[_,Y]>>(Y=Card), Copies, ActualCopies),
    append(ActualCopies, PartialDeck, FullDeck).

draw_seven(FullDeck, Deck, Hand) :-
    length(Hand, 7),
    append(Hand, Deck, FullDeck).

draw_card(PlayerName) :-
    hand(PlayerName, Hand),
    deck(PlayerName, Deck),
    (
        Deck = [],
        format("~w loses the game: deck empty when drawing!", [PlayerName]),
        halt
    ;
        Deck = [Card|Rem],
        update_state(hand, PlayerName, [Card|Hand]),
        update_state(deck, PlayerName, Rem)
    ).

discard(0, _).
discard(N, PlayerName) :-
    N #> 0,
    discard(PlayerName),
    M #= N-1,
    discard(M, PlayerName).

discard(PlayerName) :-
    hand(PlayerName, Hand),
    Hand = [H|_],
    remove_from_hand(PlayerName, H),
    put_in_graveyard(PlayerName, H).

remove_from_hand(PlayerName, CardName) :-
    hand(PlayerName, Hand),
    selectchk(CardName, Hand, NewHand),
    update_state(hand, PlayerName, NewHand).

put_in_graveyard(PlayerName, CardName) :-
    graveyard(PlayerName, Graveyard),
    update_state(graveyard, PlayerName, [CardName|Graveyard]).

shuffle(Deck, Shuffled) :-
    random_permutation(Deck, Shuffled).

play_permanent(CardInstance) :-
    CardInstance = cardinstance(_, _, _, untapped),
    board(Board),
    update_board([CardInstance|Board]).

new_card_instance(Name, Player, Instance) :-
    Instance = cardinstance(Name, Player, Player, untapped).

untap_all(ActivePlayer) :-
    board(Board),
    maplist({ActivePlayer}/[Card, NewCard]>>(
        Card = cardinstance(Name, Controller, Owner, _),
        (
            Controller = ActivePlayer,
            NewCard = cardinstance(Name, Controller, Owner, untapped)
        ;
            Controller \= ActivePlayer,
            NewCard = Card
        )
    ), Board, NewBoard),
    update_board(NewBoard).

filter_cards_include(Cards, Filters, Filtered) :-
    foldl(include, Filters, Cards, Filtered).
filter_cards_exclude(Cards, Filters, Filtered) :-
    foldl(exclude, Filters, Cards, Filtered).

has_type(Type, Name) :-
    card(Name, _, Type, _).

% helper function that keeps track of unchanged passed vars, for completeness sake
unchanged(List) :-
    maplist([X-Y]>>(X=Y), List).

% example: update_state(life, player, 15) 
% asserts life(player, 15).
update_state(State, Unit, Value) :-
    Old =.. [State, Unit, _],
    retractall(Old),
    New =.. [State, Unit, Value],
    assertz(New).

update_board(NewBoard) :-
    retractall(board(_)),
    assertz(board(NewBoard)).

%cardinstance(Name, Controller, Owner, TappedOrUntapped).
% a card is a type. token is called an cardinstance
%card(Name, Cost, Type, Effect).
