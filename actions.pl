% priority introduces the stack
action_priority(Player, NewPlayer, AI, NewAI, Board, NewBoard, ActivePlayer, Type) :-
    action_priority(Player, NewPlayer, AI, NewAI, Board, NewBoard, Type, ActivePlayer, ActivePlayer, []).

% when stack is empty, priority type resets to original
% TODO: lands are special in that they _dont_ use the stack
action_priority(Player, NewPlayer, AI, NewAI, Board, NewBoard, Type, ActivePlayer, PriorityPlayer, Stack) :-
    new_active_player(PriorityPlayer, Opponent),
    priority_type(Type, ActivePlayer, PriorityPlayer, Stack, Type1),
    choose_card_or_pass(Player, AI, Board, Type1, PriorityPlayer, Stack, CardOrPass1),
    format("Player ~w chooses ~w \n", [PriorityPlayer, CardOrPass1]),
    (
        CardOrPass1 = pass,
        priority_type(Type, ActivePlayer, Opponent, Stack, Type2),
        choose_card_or_pass(Player, AI, Board, Type2, Opponent, Stack, CardOrPass2),
        format("Player ~w chooses ~w \n", [Opponent, CardOrPass2]),
        (
            CardOrPass2 = pass,
            % both players have passed in succession
            (
                Stack = [],
                unchanged([Player-NewPlayer, AI-NewAI, Board-NewBoard])
            ;
                Stack = [H|T],
                resolve(H, Player, TempPlayer, AI, TempAI, Board, TempBoard, T, NewStack),
                action_priority(TempPlayer, NewPlayer, TempAI, NewAI, TempBoard, NewBoard, Type, ActivePlayer, ActivePlayer, NewStack)
            )
        ;
            CardOrPass2 \= pass,
            new_card_instance(CardOrPass2, Opponent, Instance),
            NewStack = [Instance|Stack],
            remove_from_players_hand(CardOrPass2, Opponent, Player, TempPlayer, AI, TempAI),
            action_priority(TempPlayer, NewPlayer, TempAI, NewAI, Board, NewBoard, Type, ActivePlayer, PriorityPlayer, NewStack)
        )
    ;
        CardOrPass1 \= pass,
        new_card_instance(CardOrPass1, PriorityPlayer, Instance),
        NewStack = [Instance|Stack],
        remove_from_players_hand(CardOrPass1, PriorityPlayer, Player, TempPlayer, AI, TempAI),
        action_priority(TempPlayer, NewPlayer, TempAI, NewAI, Board, NewBoard, Type, ActivePlayer, Opponent, NewStack)
    ).

remove_from_players_hand(CardName, PriorityPlayer, Player, NewPlayer, AI, NewAI) :-
    (
        PriorityPlayer = player,
        Player = player(Life, Hand, Deck, Graveyard),
        remove_from_hand(CardName, Hand, NewHand),
        NewPlayer = player(Life, NewHand, Deck, Graveyard),
        unchanged([AI-NewAI])
    ;
        PriorityPlayer = ai,
        AI = player(Life, Hand, Deck, Graveyard),
        remove_from_hand(CardName, Hand, NewHand),
        NewAI = player(Life, NewHand, Deck, Graveyard),
        unchanged([Player-NewPlayer])
    ).

priority_type(OriginalType, ActivePlayer, PriorityPlayer, Stack, Type) :-
    (
        ActivePlayer = PriorityPlayer, Stack = [],
        Type = OriginalType
    ;
        (ActivePlayer \= PriorityPlayer ; Stack \= []),
        Type = instant
    ).

% TODO filter out cards without legal targets or that are otherwise unplayable
choose_card_or_pass(Player, AI, Board, Type, player, Stack, Choice) :-
    Player = player(_, Hand, _, _),
    (
        Type = sorcery,
        Options = Hand
    ;
        Type = instant,
        filter_cards_include(Hand, [has_type(instant)], Options)
    ),
    (
        Options = [],
        Choice = pass
    ;
        list_to_set(Options, OptionSet),
        OptionSetPass = [pass|OptionSet],
        format("Stack: ~w\n", [Stack]),
        choose_options("Choose a card to play, or pass: ", OptionSetPass, Choice)
    ).

% TODO
choose_card_or_pass(Player, AI, Board, Type, ai, Stack, Choice) :-
    Choice = pass.

% TODO: 
resolve(Card, Player, NewPlayer, AI, NewAI, Board, NewBoard, Stack, NewStack) :-
    Card = cardinstance(CardName, Controller, _, _),
    format("Resolving ~w\n", [CardName]),
    card_details(CardName, _, Type, Rules),
    (
        % TODO: lands shouldnt use the stack at all!
        (Type = land ; Type = basic ; Type = creature ; Type = artifact ; Type = enchantment),
        play_permanent(Card, Controller, Board, NewBoard),
        unchanged([Player-NewPlayer, AI-NewAI, Stack-NewStack])
    ;
        (Type = sorcery ; Type = instant),
        put_in_graveyard(CardName, Controller, Player, NewPlayer, AI, NewAI),
        unchanged([Board-NewBoard, Stack-NewStack])
    ).

put_in_graveyard(CardName, PrioPlayer, Player, NewPlayer, AI, NewAI) :-
    (
        PrioPlayer = player,
        Player = player(Life, Hand, Deck, Graveyard),
        NewPlayer = player(Life, Hand, Deck, [CardName|Graveyard]),
        unchanged([AI-NewAI])
    ;   
        PrioPlayer = ai,
        AI = player(Life, Hand, Deck, Graveyard),
        NewAI = player(Life, Hand, Deck, [CardName|Graveyard]),
        unchanged([Player-NewPlayer])
    ).

% actions: automatic and reasoned for ai, input-based for player
% anything with choice goes in here

action_discard_down_to_max(ActivePlayer, Player, NewPlayer) :-
    Player = player(_, Hand, _, _),
    length(Hand, N),
    (
        N #=< 7,
        unchanged([Player-NewPlayer])
    ;
        N #> 7,
        X #= N - 7,
        discard(X, ActivePlayer, Player, NewPlayer)
    ).

discard(N, ActivePlayer, Player, NewPlayer) :-
    length(Loop, N),
    % TODO: how to cleanly fill a list with copies again?
    maplist({ActivePlayer}/[_,Y]>>(Y=ActivePlayer), Loop, ActualLoop),
    foldl(discard, ActualLoop, Player, NewPlayer).

discard(ActivePlayer, Player, NewPlayer) :-
    Player = player(Life, Hand, Deck, Graveyard),
    Hand = [H|T],
    NewPlayer = player(Life, T, Deck, [H|Graveyard]).
    /*
    (
        ActivePlayer = player,
        % TODO choose
        Player = NewPlayer
    ;
        ActivePlayer = ai,
        % TODO think
        Player = NewPlayer
    ).
    */

% helper function to get input from player
% Options is a list of things that is printed enumerated,
% expected player input is a digit which indicates choice
% requires input to be precise for now
% NOTE read/1 reads a prolog term, so expects a '.' character at the end!
% TODO: somehow tries to backtrack when game should end?
choose_options(Prompt, Options, Picked) :-
    write(Prompt),
    foldl(write_option, Options, 1, _),
    %read(Index),
    get_single_char(Char),
    number_codes(Index, [Char]),
    nth1(Index, Options, Picked),
    write('\n').

write_option(Option, N, M) :-
    M #= N + 1,
    format("(~w) ~w", [N, Option]).
