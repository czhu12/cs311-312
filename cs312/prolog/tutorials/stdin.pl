animal(dog).
animal(cat).
read_animal(X) :-
  write('please type animal name:'),
  nl,
  read(X),
  animal(X).
