append([], X, X).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).