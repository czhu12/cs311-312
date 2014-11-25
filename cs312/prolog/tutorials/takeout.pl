takeout(X, [X|R], R).
takeout(X, [F|S], [F|R]) :- takeout(X, S, R).
