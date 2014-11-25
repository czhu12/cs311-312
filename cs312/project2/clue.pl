% Clue Project
% Nov 28 2014
%
% Chengji Zhu
% b6k8
% 39026117
%
% John Giannakos
% g6b8
% 17698119
%
% To start game, type "start_game."
%
% Cheers!

:- dynamic holding/2.
:- dynamic question/4.
:- dynamic turn/1.
:- dynamic player_number/1.
:- dynamic number_of_players/1.
:- dynamic player/2.
:- dynamic not_holding/2.
:- dynamic definitely/1.
:- dynamic no_changes_made/0.
:- dynamic quit_clue_game/0.

person('Professor Plum').
person('Mr. Green').
person('Mrs. Peacock').
person('Mrs. White').
person('Ms. Scarlet').
person('Colonel Mustard').

weapon('knife').
weapon('candlestick').
weapon('revolver').
weapon('rope').
weapon('lead pipe').
weapon('wrench').

room('kitchen').
room('ballroom').
room('conservatory').
room('billiard room').
room('library').
room('study').
room('hall').
room('lounge').
room('dining room').

start_game :- 
    clear_all_asserts,
    get_players,get_player_names,get_player_number,get_my_cards,assert(turn(1)),lets_play,!.

cards_per_player(Number) :- number_of_players(N), (N == 2),!, Number is 9.
cards_per_player(Number) :- number_of_players(N), (N == 3),!, Number is 6.
cards_per_player(Number) :- number_of_players(N), (N == 4),!, Number is 5.
cards_per_player(Number) :- number_of_players(N), (N == 5),!, Number is 4.
cards_per_player(Number) :- number_of_players(N), (N == 6),!, Number is 3.

unknown_person(X) :- person(X),not(holding(X,_)),not(definitely(X)).
unknown_weapon(X) :- weapon(X),not(holding(X,_)),not(definitely(X)).
unknown_room(X) :- room(X),not(holding(X,_)),not(definitely(X)).

all_unknown_people(L) :- findall(X,unknown_person(X),L).
all_unknown_weapons(L) :- findall(X,unknown_weapon(X),L).
all_unknown_rooms(L) :- findall(X,unknown_room(X),L).

all_people(L) :- findall(X,person(X),L).
all_weapons(L) :- findall(X,weapon(X),L).
all_rooms(L) :- findall(X,room(X),L).
all_cards(L) :- all_people(P),all_weapons(W),all_rooms(R),append(P,W,PW),append(PW,R,L).

all_players(L) :- 
    number_of_players(X),!,numlist(1,X,N),
    maplist(player,N,L).

unassigned_person(X) :- person(X),not(player(_,X)).
unassigned_people(L) :- findall(X,unassigned_person(X),L).

all_unknown_cards(L) :- 
    all_unknown_people(P),all_unknown_weapons(W),all_unknown_rooms(R),
    append(P,W,PW),append(PW,R,L).

assert_known(C,N) :- holding(C,N),!.
assert_known(C,N) :- not(holding(C,N)),not(C == 'none'),!,assert(holding(C,N)),
    number_of_players(Num),
    findall(X,between(1,Num,X),List),
    select(N,List,NotHolding),
    players_dont_have(NotHolding,C).
assert_known(X,_) :- X == 'none',!.

% TODO: add knowns if not shown anything
assert_question(P1,(P,W,R),P2,S) :- question(P1,(P,W,R),P2,S),!.
assert_question(P1,(P,W,R),P2,S) :- not(question(P1,(P,W,R),P2,S)),!,assert(question(P1,(P,W,R),P2,S)).

clear_all_asserts :-
    retractall(holding(_,_)),retractall(not_holding(_,_)),retractall(question(_,_,_,_)),retractall(turn(_)),
    retractall(player_number(_)),retractall(number_of_players(_)),retractall(player(_,_)).

% Function to read a number between the range of min and max, inclusive
read_number(Min,Max,N) :- read(N),(N == 'entries' -> nl,print_game_state,nl,false ; check_number(Min,Max,N)).

% Helper function to check if the input is an integer between the range of min and max, inclusive
check_number(_,_,N) :-
    not(integer(N)),!,write('Error: Not a number!'),nl,nl,false.
check_number(_,Max,N) :- 
    integer(N),N > Max,!,
    write('Error: Number must be less than or equal to '),write(Max),write('!'),nl,nl,false.
check_number(Min,_,N) :- 
    integer(N),N < Min,!,
    write('Error: Number must be greater than or equal to '),write(Min),write('!'),nl,nl,false.
check_number(Min,Max,N) :- 
    integer(N),N =< Max,N >= Min,!.
    
get_players :- 
    repeat,
    write('Enter the number of players: '),
    read_number(2,6,X),assert(number_of_players(X)),nl.

get_player_names :- write('Enter players in the order of play.'),get_player_name_helper(1),nl.
get_player_name_helper(N) :- 
    number_of_players(X),N =< X,
    repeat,
    write('Who is Player '),write(N),write('?'),nl,
    unassigned_people(People),display_menu(People),length(People,Plength),
    read_number(1,Plength,Pnum),integer(Pnum),nth1(Pnum,People,Person),
    assert(player(N,Person)),
    N1 is N + 1,get_player_name_helper(N1).    
get_player_name_helper(N) :- number_of_players(X),N > X.

get_player_number :- 
    repeat,
    write('Enter your player number: '),number_of_players(N),
    read_number(1,N,X),integer(X),assert(player_number(X)),nl.
    
get_my_cards :-
    repeat,
    write('Which of the following cards do you have?'),nl,
    not_owned_list(L),display_menu(L),length(L,Clength),
    read_number(1,Clength,Cnum),integer(Cnum),nth1(Cnum,L,Card),
    store_my_card(Card),
    Card == 'None',!,nl,
    not_my_cards(L),
    true.


not_my_cards([H|_]) :- not(room(H)),not(weapon(H)),not(person(H)),!.
not_my_cards([H|T]) :- (room(H);weapon(H);person(H)),!,player_number(N),player_doesnt_have(H,N),not_my_cards(T).
    
store_my_card(C) :- C == 'None',!.
store_my_card(C) :- 
    not(C == 'None'),!,
    player_number(N),assert_known(C,N).

not_owned(C) :- all_cards(L),member(C,L),player_number(N),not(holding(C,N)).
not_owned_list(L) :- findall(X,not_owned(X),L1),append(L1,['None'],L).

display_menu(Menu) :- disp_menu(1,Menu),!.
disp_menu(_,[]).
disp_menu(N,[H|T]) :- 
    write(N),write(': '),write(H),nl,
    N1 is N + 1,disp_menu(N1,T).
    
lets_play :- repeat,retractall(quit_clue_game),handle_a_turn,(quit_clue_game ; !,true -> make_inferences,fail).

% make_inferences :- repeat,assert(no_changes_made),question_inferences,card_inferences,no_changes_made,!,true.

make_inferences :- repeat,assert(no_changes_made),!,question_inferences,card_inferences,know_players_hand,no_changes_made.

know_players_hand :- player_list_without_me(List), maplist(check_hand_and_mark_if_full,List).

check_hand_and_mark_if_full(PlayerNumber) :-    findall(C,holding(C,PlayerNumber),Holding), 
                                                length(Holding,Length),  
                                                cards_per_player(CardsperHand),
                                                (Length >= CardsperHand -> holding_no_more_cards(PlayerNumber) ; true).


holding_no_more_cards(PlayerNumber) :- all_cards(AllCards), findall(C,holding(C,PlayerNumber),Holding), subtract(AllCards, Holding, NotHolding), has_none_of_these_cards(NotHolding,PlayerNumber).

has_none_of_these_cards([],_).
has_none_of_these_cards([H|T],N) :- assert_not_holding(H,N), has_none_of_these_cards(T,N).

question_inferences :- findall((P1,(P,W,R),P2,C),variable_question(P1,(P,W,R),P2,C),QuestionList),check_questions_against_not_holding(QuestionList).

check_questions_against_not_holding([]).
check_questions_against_not_holding([(_,(P,W,R),P2,C)|T]) :- 
    CardList = [P,W,R],findall(C,not_holding(C,P2),NotHolding),subtract(CardList,NotHolding,Possible),
    (length(Possible,1) -> [Card] = Possible, assert_known(Card,P2),retractall(no_changes_made(_)) ; true),check_questions_against_not_holding(T).

variable_question(P1,(P,W,R),P2,C) :- question(P1,(P,W,R),P2,C),var(C).

card_inferences :- all_cards(CardList),check_each_card(CardList).

check_each_card([]).
check_each_card([H|T]) :- (holding(H,_) -> true; check_if_definitely_from_not_holding(H)),check_each_card(T).

check_if_definitely_from_not_holding(C) :-
    ((findall(Player,not_holding(C,Player),PlayerList),number_of_players(N),length(PlayerList,N)) -> assert_definitely(C) ; true).
    
assert_definitely(C) :- definitely(C),!.
assert_definitely(C) :- not(definitely(C)),!,assert(definitely(C)).

ask_question_with_full_menu(Question,MenuList,Answer) :-
    repeat,
    write(Question),nl,
    display_menu(MenuList),length(MenuList,MenuLength),
    read_number(1,MenuLength,MenuIndex),!,nth1(MenuIndex,MenuList,Answer).

player_string(N,Str) :- not(player_number(N)),!,player(N,Player),
    string_concat('Player ',N,Str1),string_concat(Str1,' (',Str2),string_concat(Str2,Player,Str3),string_concat(Str3,')',Str).
player_string(N,Str) :- player_number(N),!,player(N,Player),
    string_concat('You (',Player,Str1),string_concat(Str1,')',Str).

handle_a_turn :-
    TurnQ = 'What would you like to do?',get_turn_options(TurnOptions),
    ask_question_with_full_menu(TurnQ,TurnOptions,Option),nl,
    handle_a_turn_helper(Option),nl.

% Get our recommendations
handle_a_turn_helper(Option) :-
    get_turn_options(TurnOptions),
    nth1(1,TurnOptions,Option),!,
    write('I think you should ask: '),suggestion(SuggestedP,SuggestedW,SuggestedR),
    write(SuggestedP),write(' in the '),write(SuggestedR),write(' with the '),write(SuggestedW),nl.
    
% Your suggestion
handle_a_turn_helper(Option) :-
    get_turn_options(TurnOptions),
    nth1(2,TurnOptions,Option),!,
    player_number(N),player(N,Player),
    PersonQ = 'Which person did you suggest?',all_people(PersonList),
    ask_question_with_full_menu(PersonQ,PersonList,P),
    WeaponQ = 'Which weapon did you suggest?',all_weapons(WeaponList),
    ask_question_with_full_menu(WeaponQ,WeaponList,W),
    RoomQ = 'Which room did you suggest?',all_rooms(RoomList),
    ask_question_with_full_menu(RoomQ,RoomList,R),
    PlayerQ = 'Who showed you a card?',all_players(AllPlayers),
    select(Player,AllPlayers,PlayerList),append(PlayerList,['Nobody'],ShowList),
    ask_question_with_full_menu(PlayerQ,ShowList,ShowPlayer),
    get_player_number(ShowPlayer,ShowPlayerNum),
    note_silent_players(N,ShowPlayerNum,P,W,R),
    ask_for_card(ShowPlayerNum,P,W,R,Card),
    assert_known(Card,ShowPlayerNum),
    assert_question(N,(P,W,R),ShowPlayerNum,Card).

% Someone else's suggestion
handle_a_turn_helper(Option) :-
    get_turn_options(TurnOptions),
    nth1(3,TurnOptions,Option),!,
    PlayerQ = 'Who made a suggestion?',all_players(AllPlayers),
    player_number(MyN),player(MyN,MyPlayer),select(MyPlayer,AllPlayers,AllPlayersButYou),
    ask_question_with_full_menu(PlayerQ,AllPlayersButYou,Player),
    get_player_number(Player,N),player_string(N,PlayerStr),
    string_concat('Which person did ',PlayerStr,PersonQ1),string_concat(PersonQ1,' suggest?',PersonQ),
    all_people(PersonList),
    ask_question_with_full_menu(PersonQ,PersonList,P),
    string_concat('Which weapon did ',PlayerStr,WeaponQ1),string_concat(WeaponQ1,' suggest?',WeaponQ),
    all_weapons(WeaponList),
    ask_question_with_full_menu(WeaponQ,WeaponList,W),    
    string_concat('Which room did ',PlayerStr,RoomQ1),string_concat(RoomQ1,' suggest?',RoomQ),
    all_rooms(RoomList),
    ask_question_with_full_menu(RoomQ,RoomList,R),    
    string_concat('Who showed ',PlayerStr,ShowQ1),string_concat(ShowQ1,' a card?',ShowQ),
    select(Player,AllPlayers,PlayerList),append(PlayerList,['Nobody'],ShowList),
    ask_question_with_full_menu(ShowQ,ShowList,ShowPlayer),
    get_player_number(ShowPlayer,ShowPlayerNum),
    note_silent_players(N,ShowPlayerNum,P,W,R),
    assert_question(N,(P,W,R),ShowPlayerNum,_).

% Someone else's suggestion
handle_a_turn_helper(Option) :- 
    get_turn_options(TurnOptions),
    nth1(4,TurnOptions,Option),!,
    AccusationQ = 'Who made an accusation?',
    player_number(MyN),player(MyN,MyPlayer),
    all_players(AllPlayers),select(MyPlayer,AllPlayers,AllPlayersButYou),
    ask_question_with_full_menu(AccusationQ,AllPlayersButYou,Player),
    get_player_number(Player,N),player_string(N,PlayerStr),
    string_concat('Which person did ',PlayerStr,PersonQ1),string_concat(PersonQ1,' suggest?',PersonQ),
    all_people(PersonList),
    ask_question_with_full_menu(PersonQ,PersonList,P),
    string_concat('Which weapon did ',PlayerStr,WeaponQ1),string_concat(WeaponQ1,' suggest?',WeaponQ),
    all_weapons(WeaponList),
    ask_question_with_full_menu(WeaponQ,WeaponList,W),    
    string_concat('Which room did ',PlayerStr,RoomQ1),string_concat(RoomQ1,' suggest?',RoomQ),
    all_rooms(RoomList),
    ask_question_with_full_menu(RoomQ,RoomList,R),
    GameOverQ = 'Is the game over?',GameOverList=['Yes','No'],
    ask_question_with_full_menu(GameOverQ,GameOverList,Over),
    (Over == 'Yes' -> assert(quit_clue_game) ; true).

% Print entries
handle_a_turn_helper(Option) :-     
    get_turn_options(TurnOptions),
    nth1(5,TurnOptions,Option),!,
    print_game_state,nl,print_game_state_history.
    
% End game
handle_a_turn_helper(Option) :-     
    get_turn_options(TurnOptions),
    nth1(6,TurnOptions,Option),!,
    assert(quit_clue_game).

get_turn_options(L) :-
    L = [   'See Our Suggestions.',
            'Enter Your Suggestion.',
            'Enter Someone Else\'s Suggestion.',
            'Enter Someone Else\'s Accusation.',
            'View the Entries.',
            'End Game.'].
    
get_player_number(Player,Num) :- Player == 'Nobody',Num = 0.
get_player_number(Player,Num) :- not(Player == 'Nobody'),player(Num,Player).
    
ask_for_card(PlayerNum,P,W,R,Card) :-
    not(PlayerNum == 0),!,Cards = [P,W,R],
    CardQ = 'What card were you shown?',
    ask_question_with_full_menu(CardQ,Cards,Card).    
ask_for_card(PlayerNum,_,_,_,Card) :- PlayerNum == 0,!,Card = 'none'.

check_if_shown(N,P,W,R,C) :-
    player_number(N),!,turn(T),Cards = [P,W,R],
    repeat,
    write('What did player '),write(T),write(' show you?'),nl,
    display_menu(Cards),length(Cards,Clength),
    read_number(1,Clength,Cnum),integer(Cnum),nth1(Cnum,Cards,C),nl.
check_if_shown(N,_,_,_,_) :- not(player_number(N)).
    
suggestion(P,W,R) :- 
    suggested_person(P),suggested_weapon(W),suggested_room(R).

suggested_person(P) :- cards_with_hold_scores(List), suggest_first_person(List,P).
suggested_weapon(W) :- cards_with_hold_scores(List), suggest_first_weapon(List,W).
suggested_room(R) :- cards_with_hold_scores(List), suggest_first_room(List,R).

cards_with_hold_scores(List) :- all_unknown_cards(AllCards),maplist(hold_score,AllCards,Unsorted),
    sort(Unsorted,Backwards),reverse(Backwards,List).

hold_score(Card,(Score,Card)) :- findall((Card,_),not_holding(Card,_),NotHeld), 
    length(NotHeld,Len), number_of_players(N), 
    Score is (N - Len).

suggest_first_person([],E) :- E = 'Anyone'.
suggest_first_person([(_,Card)|T],E) :- (person(Card) -> E = Card ; suggest_first_person(T,E)).

suggest_first_weapon([],E) :- E = 'Anything'.
suggest_first_weapon([(_,Card)|T],E) :- (weapon(Card) -> E = Card ; suggest_first_weapon(T,E)).

suggest_first_room([],E) :- E = 'Anywhere'.
suggest_first_room([(_,Card)|T],E) :- (room(Card) -> E = Card ; suggest_first_room(T,E)).


note_silent_players(N,0,P,W,R) :- !,player_list(List),select(N,List,SilentList),players_dont_have(SilentList,P,W,R).
note_silent_players(Asker,Answerer,Person,Weapon,Room) :- silent_players(Asker,Answerer,List), players_dont_have(List,Person,Weapon,Room).

silent_players(Asker,Answerer,SilentPlayers) :- (Asker < Answerer),!, Low is Asker+1, High is Answerer-1, findall(X,between(Low,High,X),SilentPlayers).

silent_players(Asker,Answerer,SilentPlayers) :-
    (Answerer < Asker),!, Low1 is 1, High1 is Answerer-1, findall(X,between(Low1,High1,X),SilentPlayers1),
    Low2 is Asker+1,number_of_players(N),High2 is N,findall(Y,between(Low2,High2,Y),SilentPlayers2),append(SilentPlayers1,SilentPlayers2,SilentPlayers).

players_dont_have([],_,_,_).
players_dont_have([H|T],Person,Weapon,Room) :- player_doesnt_have(H,Person,Weapon,Room), players_dont_have(T,Person,Weapon,Room).

player_doesnt_have(Player,Person,Weapon,Room) :- assert_not_holding(Person,Player),assert_not_holding(Weapon,Player),assert_not_holding(Room,Player).

players_dont_have([],_).
players_dont_have([H|T],Card) :- player_doesnt_have(Card,H), players_dont_have(T,Card).

player_doesnt_have(Card,Player) :- assert_not_holding(Card,Player).

assert_not_holding(Card,Player) :- not_holding(Card,Player),!.
assert_not_holding(Card,Player) :- not(not_holding(Card,Player)),!,assert(not_holding(Card,Player)).

print_entries :- print_game_state_history, print_seen_cards.

print_game_state_history :- findall((P1,(P,W,R),P2,C),question(P1,(P,W,R),P2,C),L),print_each_turn(L).

print_each_turn([]) :- write('End of Game History.'), nl ,nl.
print_each_turn([(P1,(P,W,R),P2,C)|T]) :- print_suggestion(P1,P,W,R), print_suggestion_result(P2, C), nl, print_each_turn(T).

print_suggestion(P1,P,W,R) :- write('Player '),write(P1),write(' suggested '), write(P),write(' with the '),write(W),write(' in the '),write(R),write('.'),nl.

print_suggestion_result(P2,_) :- P2 == 0,!, write('No cards were shown!'), nl.
print_suggestion_result(P2,C) :- not(P2 == 0),!,write('Player '), write(P2), write(' showed them '), print_card(C), nl.

players_symbols_for_card([],_) :- nl.
players_symbols_for_card([H|T],Card) :- write('\t'),card_player_symbol(Card,H,Symbol),write(Symbol),players_symbols_for_card(T,Card).

card_player_symbol(Card,Player,Symbol) :- holding(Card,Player),!,Symbol = 'x'.
card_player_symbol(Card,Player,Symbol) :- not_holding(Card,Player),!,Symbol = '-'.
card_player_symbol(_,_,Symbol) :- Symbol = ' '.

player_list(List) :- number_of_players(N), numlist(1,N,List).
player_list_without_me(List) :- player_list(L),player_number(Me),select(Me,L,List).

print_card(C) :- var(C),!,write('a card').
print_card(C) :- not(var(C)),!,write(C).

print_seen_cards :- findall((C,P),holding(C,P),L), print_all_held_cards(L).
print_all_held_cards([]) :- write('End of Known Cards.'), nl, nl.
print_all_held_cards([(C,P)|T]) :- write('Player '), write(P), write(' is holding '), write(C), nl, print_all_held_cards(T).

print_game_state :- write('Here\'s how things look: \n\n'),print_player_header, print_all_card_states.

print_player_header :- player_list(List), format('Player:~23|'), print_player_numbers(List),nl.

print_player_numbers([]).
print_player_numbers([H|T]) :- write(' '),write(H),print_player_numbers(T).

print_all_card_states :- all_cards(Cards), maplist(print_card_state,Cards). 

print_card_state(Card) :- format('~w~23|',Card),player_list(Playerlist), maplist(card_player_symbol(Card),Playerlist, Symbols),format('~t'),print_player_numbers(Symbols),nl.

next_turn(N,N1) :- number_of_players(Num),N < Num,!,N1 is N + 1.
next_turn(N,N1) :- number_of_players(Num),N == Num,!,N1 is 1.
