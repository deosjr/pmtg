phase_steps(beginning, [untap, upkeep, draw]).
phase_steps(precombatmain, [pre_main_step]).
phase_steps(combat, [
    beginning_of_combat, 
    declare_attackers, 
    declare_blockers, 
    first_strike_combat_damage, 
    combat_damage, 
    end_of_combat]).
phase_steps(postcombatmain, [post_main_step]).
phase_steps(ending, [end, cleanup]).

next_phase(beginning, precombatmain).
next_phase(precombatmain, combat).
next_phase(combat, postcombatmain).
next_phase(postcombatmain, ending).
next_phase(ending, beginning).

handle_steps([], _). 
handle_steps([Step|RemSteps], ActivePlayer) :-
    call(Step, ActivePlayer),
    handle_steps(RemSteps, ActivePlayer).

untap(ActivePlayer) :-
    % phasing happens
    % active player untaps all permanents they control
    % unused mana empties from each players mana pool
    untap_all(ActivePlayer).
    
upkeep(ActivePlayer).
    % abilities triggered during the untap step and 'at the beginning of upkeep' triggered abilities trigger
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % unused mana empties from each players mana pool

draw(ActivePlayer) :-
    % active player draws a card. this is a turn-based action and does not use the stack
    % abilities triggered 'at the beginning of the draw step' and from the active player drawing a card trigger
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % unused mana empties from each players mana pool
    draw_card(ActivePlayer).

beginning_of_combat(ActivePlayer).
    % 'at the beginning of combat' triggered abilities trigger
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % unused mana empties from each players mana pool

declare_attackers(ActivePlayer).
    % active player declares their attackers. if no attackers are declared, skip declare blockers and combat damage steps
    % triggered abilities that trigger off attackers being declared trigger
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % unused mana empties from each players mana pool

declare_blockers(ActivePlayer).
    % defending player declares their blockers and which attacking creatures they will block
    % for each attacking creature that has become blocked, the active player declares the order
    % that combat damage will be assigned to blockers.
    % for each blocking creature, the defending player declares the order that combat damage
    % will be assigned to attackers (if creature can block multiple creatures)
    % triggered abilities that trigger off blockers being declared trigger
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % if a spell or ability causes a creature on the battlefield to block an attacking creature, players 
    % declare that creature's relative placement in the order that combat damage will be assigned to
    % if a creature is put onto the battlefield blocking, the active player declares its relative placement etc
    % unused mana empties from each players mana pool

first_strike_combat_damage(ActivePlayer).
    % if no attacking or blocking creatures have first or double strike, then skip this substep
    % all attacking creatures with first or double strike assign combat damage to their blockers
    % all unblocked creatures with first or double strike assign combat damage to defending player or declared planeswalkers
    % all defending creatures with first or double strike assign combat damage to their attackers
    % all assigned damage is dealt simultaneously. This does NOT use the stack, and may not be responded to.
    % 'deals combat damage' and 'is dealt combat damage' triggered abilities trigger
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % unused mana empties from each players mana pool

combat_damage(ActivePlayer).
    % exact same but for creatures without first strike (but including those with double strike!)

end_of_combat(ActivePlayer).
    % 'at end of combat' effects trigger
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % all creatures and planeswalkers are removed from combat
    % unused mana empties from each players mana pool

pre_main_step(ActivePlayer).
    % 'at the beginning of next main phase' and 'at the beginning of precombat main phase' triggered abilities trigger
    % when the stack is empty, the active player gets priority to cast spells and play lands
    % unused mana empties from each players mana pool
 
post_main_step(ActivePlayer) :-
    % 'at the beginning of next main phase' and 'at the beginning of postcombat main phase' triggered abilities trigger
    % when the stack is empty, the active player gets priority to cast spells and play lands
    % unused mana empties from each players mana pool
    action_priority(ActivePlayer, sorcery).

end(ActivePlayer).
    % 'at the beginning of the end step' or 'at the beginning of the next end step' triggered abilities trigger
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % unused mana empties from each players mana pool

cleanup(ActivePlayer) :-
    % active player discards down to their maximum hand size
    % simultaneously remove all damage from permanents and end all 'until end of turn' or 'this turn' effects
    % check for state-based actions or triggered abilities to occur, such as those that trigger 'at the beginning of the next cleanup step'
    % if nothing occurs, unused mana empties from each players mana pool and the cleanup step ends
    % active player gets priority to cast instants, spells with flash, and to use activated abilities
    % unused mana empties from each players mana pool
    % repeat the cleanup step
    action_discard_down_to_max(ActivePlayer).
