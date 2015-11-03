append(Ps, Qs, Rs) = (Ps = [] & Qs = Rs) ||
	(â X, Xs, Ys -> Ps = [X|Xs] & 
		Rs = [X|Ys] & 
			append(Xs, Qs, Ys))
