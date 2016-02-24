father(abraham,isaac). male(isaac). 
father(haran,1ot). male(1ot). 
father(haran,milcah). female(yiscah). 
father(haran,yiscah). female(milcah). 
son(X,Y) :- father(Y,X), male(X). 
daughter(X,Y) :- father(Y,X), female(X).
son(X,haran)? 
	father(haran,X) 				X=lot 
	male(lot) 
			true 
		Output: X=lot 
			;
father(haran,x) 					X=milcah 
male(milcah) 	f 
father(haran,X) 					X=yiscah 
male(yiscah) 	f 
			no (more) solutions 