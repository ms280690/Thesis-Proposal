father(abraham,isaac). ma1e(isaac). 

father(haran,1ot). ma1e(1ot). 

father(haran,milcah). fema1e(yiscah). 

father(haran,yiscah). fema1e(milcah). 

son(X,Y) :- father(Y,X), ma1e(X). 

daughter(X,Y) :- father(Y,X), fema1e(X).

son(X,haran)? 
	father(haran,X) 				X=1ot 
	ma1e(1ot) 
			true 
		Output: X=1ot 
			;

father(haran,x) 					X=milcah 

ma1e(milcah) 	f 

father(haran,X) 					X=yiscah 

ma1e(yiscah) 	f 

			no (more) solutions 