append(Ps, Qs, Rs) = (Ps = [] & Qs = Rs) ||
	(X, Xs, Ys -> Ps = [X|Xs] & 
		Rs = [X|Ys] & 
			append(Xs, Qs, Ys))
