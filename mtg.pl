:- use_module(library(clpfd)).

:- ['phases.pl'].
:- ['cards.pl'].
:- ['actions.pl'].

% only 2 player games for now: You vs AI
run :-
    PlayerDeckList = [20-"Mountain", 20-"Lava Spike"],
    AIDeckList = [20-"Island", 20-"Mind Sculpt"],
    new_player(PlayerDeckList, Player),
    new_player(AIDeckList, AI),
    % TODO: mulligans, start at begin but skip first draw
    game(Player, AI, precombatmain, [], player, 1-1).

% ActivePlayer is one of 'player' or 'ai' atoms
% Turn is TurnNum-PlayerNum pair. 
% If PlayerNum = NumPlayers, everyone has had a turn and a new turn begins
game(Player, AI, Phase, Board, ActivePlayer, Turn) :-
    print_gamestate(Player, AI, Phase, Board, ActivePlayer, Turn),
    % sanity checks and unwrapping of fields
    Player = player(_,_,_,_),
    AI = player(_,_,_,_),
    phase_steps(Phase, Steps),
    handle_steps(Steps, Player, NewPlayer, AI, NewAI, Board, NewBoard, ActivePlayer),
    next_phase(Phase, NextPhase),
    % turn rollover
    (
        NextPhase = beginning,
        new_active_player(ActivePlayer, NewActivePlayer),
        (
            Turn = N-1,
            game(NewPlayer, NewAI, NextPhase, NewBoard, NewActivePlayer, N-2)
        ;
            Turn = N-2,
            NewN #= N+1,
            game(NewPlayer, NewAI, NextPhase, NewBoard, NewActivePlayer, NewN-1)
        )
    ;
    % next phase within a turn
        NextPhase \= beginning,
        game(NewPlayer, NewAI, NextPhase, NewBoard, ActivePlayer, Turn)
    ).

print_gamestate(Player, AI, Phase, Board, ActivePlayer, Turn-_) :-
    tty_clear,
    tty_goto(0, 0),
    format("Turn ~w ~w phase. Active player: ~w\n", [Turn, Phase, ActivePlayer]),
    Player = player(PLife, PHand, PDeck, PGraveyard),
    length(PDeck, PN),
    format("Player life: ~w\nPlayer hand: ~w\nPlayer decksize: ~w\nPlayer graveyard: ~w\n", [PLife, PHand, PN, PGraveyard]),
    AI = player(AILife, AIHand, AIDeck, AIGraveyard),
    length(AIHand, AIH),
    length(AIDeck, AIN),
    format("AI life: ~w\nAI handsize: ~w\nAI decksize: ~w\nAI graveyard: ~w\n", [AILife, AIH, AIN, AIGraveyard]),
    format("Board: ~w\n", [Board]),
    format("-----------------------------------------------------------------------\n", []).

new_active_player(player, ai).
new_active_player(ai, player).

% a deck is a sorted list of cards.
% dont have to be instanced, so its a list of cardnames only
% same for a hand, except it doesnt even have to be sorted
% a decklist is a list of number-cardname pairs where each card
% is included #number times in the deck
%player(Life, Hand, Deck, Graveyard).

new_player(Decklist, Player) :-
    forall(member(_-Card, Decklist), (
        card(Card, _, _, _) ;
        (format("card not found: ~w\n", [Card]), fail)
    )),
    decklist_to_deck(Decklist, FullDeck),
    shuffle(FullDeck, ShuffledDeck),
    draw_seven(ShuffledDeck, Deck, Hand),
    Player = player(20, Hand, Deck, []).

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

draw(Player, NewPlayer) :-
    Player = player(Life, Hand, [Draw|Deck], Graveyard),
    NewPlayer = player(Life, [Draw|Hand], Deck, Graveyard). 

remove_from_hand(Name, Hand, NewHand) :-
    selectchk(Name, Hand, NewHand).

shuffle(Deck, Shuffled) :-
    random_permutation(Deck, Shuffled).

play_permanent(CardName, PlayerName, Board, NewBoard) :-
    CardInstance = cardinstance(CardName, PlayerName, PlayerName, untapped),
    NewBoard = [CardInstance|Board].

new_card_instance(Name, Player, Instance) :-
    Instance = cardinstance(Name, Player, Player, untapped).

untap_all(ActivePlayer, Board, NewBoard) :-
    maplist({ActivePlayer}/[Card, NewCard]>>(
        Card = cardinstance(Name, Controller, Owner, _),
        (
            Controller = ActivePlayer,
            NewCard = cardinstance(Name, Controller, Owner, untapped)
        ;
            Controller \= ActivePlayer,
            NewCard = Card
        )
    ), Board, NewBoard).


filter_cards_include(Cards, Filters, Filtered) :-
    foldl(include, Filters, Cards, Filtered).
filter_cards_exclude(Cards, Filters, Filtered) :-
    foldl(exclude, Filters, Cards, Filtered).

has_type(Type, Name) :-
    card(Name, _, Type, _).

% helper function that keeps track of unchanged passed vars, for completeness sake
unchanged(List) :-
    maplist([X-Y]>>(X=Y), List).

%cardinstance(Name, Controller, Owner, TappedOrUntapped).
% a card is a type. token is called an cardinstance
%card(Name, Cost, Type, Effect).
