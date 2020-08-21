grid_build(N,M):-
    length(M,N),
    grid_buildH(N,M,0). 


grid_buildH(N,[],N).


grid_buildH(N,[H|T],Count):-

       Count<N,
       length(H,N),
	   Count1 is Count+1,
	   grid_buildH(N,T,Count1).


num_gen(X,X1,[]):-
         X is X1 +1.

num_gen(F,L,[H|T]):-
     F=<L,
     H = F,
	 F1 is F +1,
	 num_gen(F1,L,T).


grid_gen(N,G):- 
    ground(G),grid_genH2(N,G).
grid_genH2(N,M):-
    grid_build(N,M),
	check2(N,M).	
check2(N,[]).
	
check2(N,[H|T]):-
   checkH2(N,H),
   check2(N,T).
	
checkH2(N,[]).
 
 checkH2(N,[H|T]):-
    num_gen(1,N,L),
    member(H,L),
	checkH2(N,T).
	
grid_gen(N,G ):-
\+ground(G),
    grid_build(N,M), 
	grid_genH(N, M,[], G) .
	   
 
   
grid_genH(_,[],R,R).
grid_genH(N,[H|T],R,M):-
      
	check(N,H),
    compress2(H,[],L1),
	checkList(L1,N),
	\+member(H,R),
	append([H],R,R1), 
	grid_genH(N,T,R1,M).
	   
	
check(N,[]).
check(N,[H|T]):-
	num_gen(1,N,L),
	member(H,L),
	check(N,T).
	 
	 
	
check_num_grid(G):-
	length(G,C),
	check_num_gridH(G,C).
	

check_num_gridH([H|T],C):-
    compress([H|T],L),compress2(L,[],L1),checkList(L1,C).


compress([],[]).
	
compress([H|T],L):-
append(H,L1,L),
compress(T,L1).

compress2([],L,L).

compress2([H|T],Acc,L1):-
member(H,Acc),compress2(T,Acc,L1).

compress2([H|T],Acc,L1):-
\+member(H,Acc),append(Acc,[H],NewAcc),compress2(T,NewAcc,L1).




     

checkList(L,C):-
    max_member(Max,L),Max=<C,
	num_gen(1,Max,Z),permutation(Z,L).
	
	
trans(Org,Trans):-

 length(Org,N),
 creat(N,Trans1),
 transH(Org,Trans1,Trans).


creat(N,L):-
creat(0,N,[],L).

creat(N,N,L,L).


creat(Count,N,ACC,L):-
      Count<N,
	  append(ACC,[[]],NEWACC),
       COUNT1 is Count+1,
       creat(COUNT1,N,NEWACC,L).	  
     
transH([],T,T).

transH([H|T],Acc,Trans):-
trans2(H,Tranc1,Trans),
transH(T,Acc,Tranc1).




trans2([],_,[]).



trans2([H|T],[H1|T1],[H2|T2]):-
append([H],H1,H2),
trans2(T,T1,T2).





acceptable_distribution(G):-
    trans(G,GT),
acceptable_distributionH(G,GT).


acceptable_distributionH([],[]).

acceptable_distributionH([H1|T1],[H2|T2]):-
     H1\=H2,
acceptable_distributionH(T1,T2).

    

distinct_rows(M):-
rowChecker(M,[[]]).



rowChecker([],_).

rowChecker([H|T],L):-
\+member(H,L),
append([H],L,L1),
rowChecker(T,L1).


distinct_columns(M):-
trans(M,MT),
distinct_rows(MT).


row_col_match(M):-
trans(M,MT),
row_col_matchH(M,MT).


row_col_matchH([],_).


row_col_matchH([H1|T1],MT):-

member(H1,MT),
row_col_matchH(T1,MT).



acceptable_permutation(L,T):-
permutation(L,T),
checkp(L,T).


checkp([],[]).
checkp([H|T],[H1|T1]):-
H\=H1,
checkp(T,T1).




helsinki(N,G):-ground(G),
helsinkiH(N,G).
  
helsinki(N,G):- 
\+ground(G),grid_gen(N,G),check_num_grid(G),acceptable_distribution(G),distinct_rows(G),distinct_columns(G),row_col_match(G).
  
 

helsinkiH(N,[H1|T1]):- 
G=[H1|T1],
 check_num_grid(G),acceptable_distribution(G),distinct_rows(G),distinct_columns(G),row_col_match(G) . 



	

	  
	  
	  
	  
        