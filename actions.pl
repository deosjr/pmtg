% priority introduces the stack
action_priority(ActivePlayer, Type) :-
    action_priority(Type, ActivePlayer, ActivePlayer, []).

% when stack is empty, priority type resets to original
% TODO: lands are special in that they _dont_ use the stack
action_priority(Type, ActivePlayer, PriorityPlayer, Stack) :-
    other_player(PriorityPlayer, Opponent),
    priority_type(Type, ActivePlayer, PriorityPlayer, Stack, Type1),
    choose_card_or_pass(Type1, PriorityPlayer, Stack, CardOrPass1),
    format("Player ~w chooses ~w \n", [PriorityPlayer, CardOrPass1]),
    (
        CardOrPass1 = pass,
        priority_type(Type, ActivePlayer, Opponent, Stack, Type2),
        choose_card_or_pass(Type2, Opponent, Stack, CardOrPass2),
        format("Player ~w chooses ~w \n", [Opponent, CardOrPass2]),
        (
            CardOrPass2 = pass,
            % both players have passed in succession
            (
                Stack = []
            ;
                Stack = [H|T],
                resolve(H, T, NewStack),
                action_priority(Type, ActivePlayer, ActivePlayer, NewStack)
            )
        ;
            CardOrPass2 \= pass,
            new_card_instance(CardOrPass2, Opponent, Instance),
            NewStack = [Instance|Stack],
            remove_from_hand(Opponent, CardOrPass2),
            action_priority(Type, ActivePlayer, PriorityPlayer, NewStack)
        )
    ;
        CardOrPass1 \= pass,
        new_card_instance(CardOrPass1, PriorityPlayer, Instance),
        NewStack = [Instance|Stack],
        remove_from_hand(PriorityPlayer, CardOrPass1),
        action_priority(Type, ActivePlayer, Opponent, NewStack)
    ).

priority_type(OriginalType, ActivePlayer, PriorityPlayer, Stack, Type) :-
    (
        ActivePlayer = PriorityPlayer, Stack = [],
        Type = OriginalType
    ;
        (ActivePlayer \= PriorityPlayer ; Stack = [_|_]),
        Type = instant
    ).

% TODO filter out cards without legal targets or that are otherwise unplayable
% not even considering opponent or boardstate yet
choose_card_or_pass(Type, player, Stack, Choice) :-
    hand(player, Hand),
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
        Options = [_|_],
        list_to_set(Options, OptionSet),
        OptionSetPass = [pass|OptionSet],
        format("Stack: ~w\n", [Stack]),
        choose_options("Choose a card to play, or pass: ", OptionSetPass, Choice)
    ).

% TODO
choose_card_or_pass(Type, ai, Stack, Choice) :-
    Choice = pass.

% TODO: 
% NewStack only changes when things resolve that affect the stack
% such as countermagic
resolve(Card, RestOfStack, NewStack) :-
    Card = cardinstance(CardName, Controller, _, _),
    format("Resolving ~w\n", [CardName]),
    card_details(CardName, _, Type, Rules),
    (
        % TODO: lands shouldnt use the stack at all!
        (Type = land ; Type = basic ; Type = creature ; Type = artifact ; Type = enchantment),
        play_permanent(Card, Controller)
    ;
        (Type = sorcery ; Type = instant),
        put_in_graveyard(CardName, Controller)
    ),
    unchanged([RestOfStack-NewStack]).

% actions: automatic and reasoned for ai, input-based for player
% anything with choice goes in here

action_discard_down_to_max(PlayerName) :-
    hand(PlayerName, Hand),
    length(Hand, N),
    (
        N #=< 7
    ;
        N #> 7,
        X #= N - 7,
        discard(X, PlayerName)
    ).

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
