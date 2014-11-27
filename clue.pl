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

suspect('S - Professor Plum').
suspect('S - Mr Green').
suspect('S - Mrs Peacock').
suspect('S - Mrs White').
suspect('S - Ms Scarlet').
suspect('S - Corlonel Mustard').

weapon('W - Knife').
weapon('W - Candlestick').
weapon('W - Revolver').
weapon('W - Rope').
weapon('W - Lead Pipe').
weapon('W - Wrench').

room('R - Kitchen').
room('R - Ballroom').
room('R - Conservatory').
room('R - Billiard room').
room('R - Library').
room('R - Study').
room('R - Hall').
room('R - Lounge').
room('R - Dining Room').

start_game :- 
    clear_all_asserts,
    get_suspects,get_suspect_names,get_suspect_number,get_my_cards,assert(turn(1)),lets_play,!.

cards_per_suspect(Number) :- num_suspects(N), (N == 2),!, Number is 9.
cards_per_suspect(Number) :- num_suspects(N), (N == 3),!, Number is 6.
cards_per_suspect(Number) :- num_suspects(N), (N == 4),!, Number is 5.
cards_per_suspect(Number) :- num_suspects(N), (N == 5),!, Number is 4.
cards_per_suspect(Number) :- num_suspects(N), (N == 6),!, Number is 3.

unknown_suspect(X) :- suspect(X),not(is_holding(X,_)),not(definitely(X)).
unknown_weapon(X) :- weapon(X),not(is_holding(X,_)),not(definitely(X)).
unknown_room(X) :- room(X),not(is_holding(X,_)),not(definitely(X)).

all_unknown_people(L) :- findall(X,unknown_suspect(X),L).
all_unknown_weapons(L) :- findall(X,unknown_weapon(X),L).
all_unknown_rooms(L) :- findall(X,unknown_room(X),L).

all_people(L) :- findall(X,suspect(X),L).
all_weapons(L) :- findall(X,weapon(X),L).
all_rooms(L) :- findall(X,room(X),L).
all_cards(L) :- all_people(P),all_weapons(W),all_rooms(R),append(P,W,PW),append(PW,R,L).

all_suspects(L) :- 
    num_suspects(X),!,numlist(1,X,N),
    maplist(suspect,N,L).

unassigned_suspect(X) :- suspect(X),not(suspect(_,X)).
unassigned_people(L) :- findall(X,unassigned_suspect(X),L).

all_unknown_cards(L) :- 
    all_unknown_people(P),all_unknown_weapons(W),all_unknown_rooms(R),
    append(P,W,PW),append(PW,R,L).

assert_known(C,N) :- is_holding(C,N),!.
assert_known(C,N) :- not(is_holding(C,N)),not(C == 'none'),!,assert(is_holding(C,N)),
    num_suspects(Num),
    findall(X,between(1,Num,X),List),
    select(N,List,NotHolding),
    suspects_dont_have(NotHolding,C).
assert_known(X,_) :- X == 'none',!.

assert_question(P1,(P,W,R),P2,S) :- question(P1,(P,W,R),P2,S),!.
assert_question(P1,(P,W,R),P2,S) :- not(question(P1,(P,W,R),P2,S)),!,assert(question(P1,(P,W,R),P2,S)).

clear_all_asserts :-
    retractall(is_holding(_,_)),retractall(is_not_holding(_,_)),retractall(question(_,_,_,_)),retractall(turn(_)),
    retractall(suspect_number(_)),retractall(num_suspects(_)),retractall(suspect(_,_)).

read_number(Min,Max,N) :- read(N),(N == 'entries' -> nl,print_game_state,nl,false ; check_number(Min,Max,N)).

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
    
get_suspects :- 
    repeat,
    print_welcome_message,
    write('Enter the number of suspects: '),
    read_number(2,6,X),assert(num_suspects(X)),nl.

print_welcome_message :- 
    repeat,nl,nl,
    write('========================================'),nl,
    write('Welcome to Clue 2.0'),nl,
    write('By: Chris Zhu and John Giannokos'),nl,
    write('========================================'),nl,nl,
    write('We can help you keep track of what has been seen, as well as make suggestions for what you can do.'), nl,nl,nl,
    write('Good Luck!'), nl.


get_suspect_names :- write('Enter suspects in the order turns are taken. '),get_suspect_name_helper(1),nl.
get_suspect_name_helper(N) :- 
    num_suspects(X),N =< X,
    repeat,
    write('Who is Suspect '),write(N),write('?'),nl,
    unassigned_people(People),display_menu(People),length(People,Plength),
    read_number(1,Plength,Pnum),integer(Pnum),nth1(Pnum,People,Person),
    assert(suspect(N,Person)),
    N1 is N + 1,get_suspect_name_helper(N1).    
get_suspect_name_helper(N) :- num_suspects(X),N > X.

get_suspect_number :- 
    repeat,
    write('What is your index number in the suspects you just entered? '),num_suspects(N),
    read_number(1,N,X),integer(X),assert(suspect_number(X)),nl.
    
get_my_cards :-
    repeat, write('Which do you have?'),nl,
    not_owned_list(L),display_menu(L),length(L,Clength),
    read_number(1,Clength,Cnum),integer(Cnum),nth1(Cnum,L,Card),
    store_my_card(Card),
    Card == 'Done',!,nl,
    not_my_cards(L),
    true.

not_owned(C) :- all_cards(L),member(C,L),suspect_number(N),not(is_holding(C,N)).
not_owned_list(L) :- findall(X,not_owned(X),L1),append(L1,['Done'],L).

display_menu(Menu) :- disp_menu(1,Menu),!.
disp_menu(_,[]).
disp_menu(N,[H|T]) :- 
    write(N),write(': '),write(H),nl,
    N1 is N + 1,disp_menu(N1,T).
    
lets_play :- repeat,retractall(quit_clue_game),next_round,(quit_clue_game ; !,true -> make_inferences,fail).

% make_inferences :- repeat,assert(nothing_changed),question_inferences,card_inferences,nothing_changed,!,true.

make_inferences :- repeat,assert(nothing_changed),!,question_inferences,card_inferences,know_suspects_hand,nothing_changed.

know_suspects_hand :- suspect_list_without_me(List), maplist(check_hand_and_mark_if_full,List).

check_hand_and_mark_if_full(SuspectNumber) :-    findall(C,is_holding(C,SuspectNumber),Holding), 
                                                length(Holding,Length),  
                                                cards_per_suspect(CardsperHand),
                                                (Length >= CardsperHand -> is_holding_no_more_cards(SuspectNumber) ; true).


is_holding_no_more_cards(SuspectNumber) :- all_cards(AllCards), findall(C,is_holding(C,SuspectNumber),Holding), subtract(AllCards, Holding, NotHolding), has_none_of_these_cards(NotHolding,SuspectNumber).

has_none_of_these_cards([],_).
has_none_of_these_cards([H|T],N) :- assert_is_not_holding(H,N), has_none_of_these_cards(T,N).

question_inferences :- findall((P1,(P,W,R),P2,C),variable_question(P1,(P,W,R),P2,C),QuestionList),check_questions_against_is_not_holding(QuestionList).

check_questions_against_is_not_holding([]).
check_questions_against_is_not_holding([(_,(P,W,R),P2,C)|T]) :- 
    CardList = [P,W,R],findall(C,is_not_holding(C,P2),NotHolding),subtract(CardList,NotHolding,Possible),
    (length(Possible,1) -> [Card] = Possible, assert_known(Card,P2),retractall(nothing_changed(_)) ; true),check_questions_against_is_not_holding(T).

variable_question(P1,(P,W,R),P2,C) :- question(P1,(P,W,R),P2,C),var(C).

card_inferences :- all_cards(CardList),check_each_card(CardList).

check_each_card([]).
check_each_card([H|T]) :- (is_holding(H,_) -> true; check_if_definitely_from_is_not_holding(H)),check_each_card(T).

check_if_definitely_from_is_not_holding(C) :-
    ((findall(Suspect,is_not_holding(C,Suspect),SuspectList),num_suspects(N),length(SuspectList,N)) -> assert_definitely(C) ; true).
    
assert_definitely(C) :- definitely(C),!.
assert_definitely(C) :- not(definitely(C)),!,assert(definitely(C)).

ask_question_with_full_menu(Question,MenuList,Answer) :-
    repeat,
    write(Question),nl,
    display_menu(MenuList),length(MenuList,MenuLength),
    read_number(1,MenuLength,MenuIndex),!,nth1(MenuIndex,MenuList,Answer).

suspect_str(N,Str) :- not(suspect_number(N)),!,suspect(N,Suspect),
    string_concat('Suspect ',N,Str1),string_concat(Str1,' (',Str2),string_concat(Str2,Suspect,Str3),string_concat(Str3,')',Str).
suspect_str(N,Str) :- suspect_number(N),!,suspect(N,Suspect),
    string_concat('You (',Suspect,Str1),string_concat(Str1,')',Str).

next_round :-
    TurnQ = 'What would you like to do?',get_turn_options(TurnOptions),
    ask_question_with_full_menu(TurnQ,TurnOptions,Option),nl,
    next_round_helper(Option),nl.

% Get our recommendations
next_round_helper(Option) :-
    get_turn_options(TurnOptions),
    nth1(1,TurnOptions,Option),!,
    write('I think you should ask: '),suggestion(SuggestedP,SuggestedW,SuggestedR),
    write(SuggestedP),write(' in the '),write(SuggestedR),write(' with the '),write(SuggestedW),nl.
    
% Your suggestion
next_round_helper(Option) :-
    get_turn_options(TurnOptions),
    nth1(2,TurnOptions,Option),!,
    suspect_number(N),suspect(N,Suspect),
    PersonQ = 'Which suspect?',all_people(PersonList),
    ask_question_with_full_menu(PersonQ,PersonList,P),
    WeaponQ = 'Which weapon?',all_weapons(WeaponList),
    ask_question_with_full_menu(WeaponQ,WeaponList,W),
    RoomQ = 'Which room?',all_rooms(RoomList),
    ask_question_with_full_menu(RoomQ,RoomList,R),
    SuspectQ = 'Who showed you their card?',all_suspects(AllSuspects),
    select(Suspect,AllSuspects,SuspectList),append(SuspectList,['Nobody'],ShowList),
    ask_question_with_full_menu(SuspectQ,ShowList,ShowSuspect),
    get_suspect_number(ShowSuspect,ShowSuspectNum),
    note_silent_suspects(N,ShowSuspectNum,P,W,R),
    request_card(ShowSuspectNum,P,W,R,Card),
    assert_known(Card,ShowSuspectNum),
    assert_question(N,(P,W,R),ShowSuspectNum,Card).

% Someone else's suggestion
next_round_helper(Option) :-
    get_turn_options(TurnOptions),
    nth1(3,TurnOptions,Option),!,
    SuspectQ = 'Whos turn was it?',all_suspects(AllSuspects),
    suspect_number(MyN),suspect(MyN,MySuspect),select(MySuspect,AllSuspects,AllSuspectsButYou),
    ask_question_with_full_menu(SuspectQ,AllSuspectsButYou,Suspect),
    get_suspect_number(Suspect,N),suspect_str(N,SuspectStr),
    string_concat('Which suspect did ',SuspectStr,PersonQ1),string_concat(PersonQ1,' ask for ?',PersonQ),
    all_people(PersonList),
    ask_question_with_full_menu(PersonQ,PersonList,P),
    string_concat('Which weapon did ',SuspectStr,WeaponQ1),string_concat(WeaponQ1,' ask for?',WeaponQ),
    all_weapons(WeaponList),
    ask_question_with_full_menu(WeaponQ,WeaponList,W),    
    string_concat('Which room did ',SuspectStr,RoomQ1),string_concat(RoomQ1,' ask for?',RoomQ),
    all_rooms(RoomList),
    ask_question_with_full_menu(RoomQ,RoomList,R),    
    string_concat('Who showed ',SuspectStr,ShowQ1),string_concat(ShowQ1,' a card?',ShowQ),
    select(Suspect,AllSuspects,SuspectList),append(SuspectList,['Nobody'],ShowList),
    ask_question_with_full_menu(ShowQ,ShowList,ShowSuspect),
    get_suspect_number(ShowSuspect,ShowSuspectNum),
    note_silent_suspects(N,ShowSuspectNum,P,W,R),
    assert_question(N,(P,W,R),ShowSuspectNum,_).

% Someone else's suggestion
next_round_helper(Option) :- 
    get_turn_options(TurnOptions),
    nth1(4,TurnOptions,Option),!,
    AccusationQ = 'Who made an accusation?',
    suspect_number(MyN),suspect(MyN,MySuspect),
    all_suspects(AllSuspects),select(MySuspect,AllSuspects,AllSuspectsButYou),
    ask_question_with_full_menu(AccusationQ,AllSuspectsButYou,Suspect),
    get_suspect_number(Suspect,N),suspect_str(N,SuspectStr),
    string_concat('Which suspect did ',SuspectStr,PersonQ1),string_concat(PersonQ1,' ask for?',PersonQ),
    all_people(PersonList),
    ask_question_with_full_menu(PersonQ,PersonList,P),
    string_concat('Which weapon did ',SuspectStr,WeaponQ1),string_concat(WeaponQ1,' ask for?',WeaponQ),
    all_weapons(WeaponList),
    ask_question_with_full_menu(WeaponQ,WeaponList,W),    
    string_concat('Which room did ',SuspectStr,RoomQ1),string_concat(RoomQ1,' ask for?',RoomQ),
    all_rooms(RoomList),
    ask_question_with_full_menu(RoomQ,RoomList,R),
    GameOverQ = 'Did they get it?',GameOverList=['Yes','No'],
    ask_question_with_full_menu(GameOverQ,GameOverList,Over),
    (Over == 'Yes' -> assert(quit_clue_game) ; true).

% Print entries
next_round_helper(Option) :-     
    get_turn_options(TurnOptions),
    nth1(5,TurnOptions,Option),!,
    print_game_state,nl,print_game_state_history.
    
% End game
next_round_helper(Option) :-     
    get_turn_options(TurnOptions),
    nth1(6,TurnOptions,Option),!,
    assert(quit_clue_game).

get_turn_options(L) :-
    L = [   'Type "1." to and we\'ll give you our suggestion. ',
            'Type "2." to enter your suggestion and we\'ll track it for you.',
            'Type "3." to enter someone else\'s suggestion and we\'ll track it for you.',
            'Type "4." to enter someone else\'s accusation and we\'ll track it for you.',
            'Type "5." to see what we have saved so far.',
            'Type "6." to exit.'].
    
get_suspect_number(Suspect,Num) :- Suspect == 'Nobody',Num = 0.
get_suspect_number(Suspect,Num) :- not(Suspect == 'Nobody'),suspect(Num,Suspect).

check_if_shown(N,P,W,R,C) :-
    suspect_number(N),!,turn(T),Cards = [P,W,R],
    repeat,
    write('What did suspect '),write(T),write(' show?'),nl,
    display_menu(Cards),length(Cards,Clength),
    read_number(1,Clength,Cnum),integer(Cnum),nth1(Cnum,Cards,C),nl.
check_if_shown(N,_,_,_,_) :- not(suspect_number(N)).
    
suggestion(P,W,R) :- 
    suggested_suspect(P),suggested_weapon(W),suggested_room(R).

suggested_suspect(P) :- cards_with_hold_scores(List), suggest_first_suspect(List,P).
suggested_weapon(W) :- cards_with_hold_scores(List), suggest_first_weapon(List,W).
suggested_room(R) :- cards_with_hold_scores(List), suggest_first_room(List,R).

cards_with_hold_scores(List) :- all_unknown_cards(AllCards),maplist(hold_score,AllCards,Unsorted),
    sort(Unsorted,Backwards),reverse(Backwards,List).

hold_score(Card,(Score,Card)) :- findall((Card,_),is_not_holding(Card,_),NotHeld), 
    length(NotHeld,Len), num_suspects(N), 
    Score is (N - Len).

suggest_first_suspect([],E) :- E = 'Any Person'.
suggest_first_suspect([(_,Card)|T],E) :- (suspect(Card) -> E = Card ; suggest_first_suspect(T,E)).

suggest_first_weapon([],E) :- E = 'Any Weapon'.
suggest_first_weapon([(_,Card)|T],E) :- (weapon(Card) -> E = Card ; suggest_first_weapon(T,E)).

suggest_first_room([],E) :- E = 'Any Room.'.
suggest_first_room([(_,Card)|T],E) :- (room(Card) -> E = Card ; suggest_first_room(T,E)).


note_silent_suspects(N,0,P,W,R) :- !,suspect_list(List),select(N,List,SilentList),suspects_dont_have(SilentList,P,W,R).
note_silent_suspects(Asker,Answerer,Person,Weapon,Room) :- silent_suspects(Asker,Answerer,List), suspects_dont_have(List,Person,Weapon,Room).

silent_suspects(Asker,Answerer,SilentSuspects) :- (Asker < Answerer),!, Low is Asker+1, High is Answerer-1, findall(X,between(Low,High,X),SilentSuspects).

silent_suspects(Asker,Answerer,SilentSuspects) :-
    (Answerer < Asker),!, Low1 is 1, High1 is Answerer-1, findall(X,between(Low1,High1,X),SilentSuspects1),
    Low2 is Asker+1,num_suspects(N),High2 is N,findall(Y,between(Low2,High2,Y),SilentSuspects2),append(SilentSuspects1,SilentSuspects2,SilentSuspects).

suspects_dont_have([],_,_,_).
suspects_dont_have([H|T],Person,Weapon,Room) :- suspect_doesnt_have(H,Person,Weapon,Room), suspects_dont_have(T,Person,Weapon,Room).

suspect_doesnt_have(Suspect,Person,Weapon,Room) :- assert_is_not_holding(Person,Suspect),assert_is_not_holding(Weapon,Suspect),assert_is_not_holding(Room,Suspect).

suspects_dont_have([],_).
suspects_dont_have([H|T],Card) :- suspect_doesnt_have(Card,H), suspects_dont_have(T,Card).

suspect_doesnt_have(Card,Suspect) :- assert_is_not_holding(Card,Suspect).
suspect_list(List) :- num_suspects(N), numlist(1,N,List).
suspect_list_without_me(List) :- suspect_list(L),suspect_number(Me),select(Me,L,List).

print_card(C) :- var(C),!,write('a card').
print_card(C) :- not(var(C)),!,write(C).

print_seen_cards :- findall((C,P),is_holding(C,P),L), print_all_held_cards(L).
print_all_held_cards([]) :- write('End of Known Cards.'), nl, nl.
print_all_held_cards([(C,P)|T]) :- write('Suspect '), write(P), write(' is is_holding '), write(C), nl, print_all_held_cards(T).

print_game_state :- write('Here\'s how things look: \n\n'),print_suspect_header, print_all_card_states.

print_suspect_header :- suspect_list(List), format('Suspect:~23|'), print_suspect_numbers(List),nl.

print_suspect_numbers([]).
print_suspect_numbers([H|T]) :- write(' '),write(H),print_suspect_numbers(T).

print_all_card_states :- all_cards(Cards), maplist(print_card_state,Cards). 

print_card_state(Card) :- format('~w~23|',Card),suspect_list(Suspectlist), maplist(card_suspect_symbol(Card),Suspectlist, Symbols),format('~t'),print_suspect_numbers(Symbols),nl.
    
request_card(SuspectNum,P,W,R,Card) :-
    not(SuspectNum == 0),!,Cards = [P,W,R],
    CardQ = 'What card were you shown?',
    ask_question_with_full_menu(CardQ,Cards,Card).    
request_card(SuspectNum,_,_,_,Card) :- SuspectNum == 0,!,Card = 'none'.

next_turn(N,N1) :- num_suspects(Num),N < Num,!,N1 is N + 1.
next_turn(N,N1) :- num_suspects(Num),N == Num,!,N1 is 1.

assert_is_not_holding(Card,Suspect) :- is_not_holding(Card,Suspect),!.
assert_is_not_holding(Card,Suspect) :- not(is_not_holding(Card,Suspect)),!,assert(is_not_holding(Card,Suspect)).

print_entries :- print_game_state_history, print_seen_cards.

print_game_state_history :- findall((P1,(P,W,R),P2,C),question(P1,(P,W,R),P2,C),L),print_each_turn(L).

print_each_turn([]) :- write('End of Game History.'), nl ,nl.
print_each_turn([(P1,(P,W,R),P2,C)|T]) :- print_suggestion(P1,P,W,R), print_suggestion_result(P2, C), nl, print_each_turn(T).

print_suggestion(P1,P,W,R) :- write('Suspect '),write(P1),write(' suggested '), write(P),write(' with the '),write(W),write(' in the '),write(R),write('.'),nl.

print_suggestion_result(P2,_) :- P2 == 0,!, write('No cards were shown!'), nl.
print_suggestion_result(P2,C) :- not(P2 == 0),!,write('Suspect '), write(P2), write(' showed them '), print_card(C), nl.

suspects_symbols_for_card([],_) :- nl.
suspects_symbols_for_card([H|T],Card) :- write('\t'),card_suspect_symbol(Card,H,Symbol),write(Symbol),suspects_symbols_for_card(T,Card).

card_suspect_symbol(Card,Suspect,Symbol) :- is_holding(Card,Suspect),!,Symbol = 'x'.
card_suspect_symbol(Card,Suspect,Symbol) :- is_not_holding(Card,Suspect),!,Symbol = '-'.
card_suspect_symbol(_,_,Symbol) :- Symbol = ' '.

not_my_cards([H|_]) :- not(room(H)),not(weapon(H)),not(suspect(H)),!.
not_my_cards([H|T]) :- (room(H);weapon(H);suspect(H)),!,suspect_number(N),suspect_doesnt_have(H,N),not_my_cards(T).
    
store_my_card(C) :- C == 'Done',!.
store_my_card(C) :- 
    not(C == 'Done'),!,
    suspect_number(N),assert_known(C,N).


:- dynamic is_holding/2.
:- dynamic question/4.
:- dynamic turn/1.
:- dynamic suspect_number/1.
:- dynamic num_suspects/1.
:- dynamic suspect/2.
:- dynamic is_not_holding/2.
:- dynamic definitely/1.
:- dynamic nothing_changed/0.
:- dynamic quit_clue_game/0.
