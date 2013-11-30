#lang reader "../racklog/reader.rkt"

:- module(test,[test/0, test2/0]).
:- use_module("../racklog/utils.rkt").

foo :- bar(A).
bar(A).

queens([]).                                  % when place queen in empty list, solution found
 
queens([ Row/Col | Rest]) :-                 % otherwise, for each row
            queens(Rest),                    % place a queen in each higher numbered row
            member(Col, [1,2,3,4,5,6,7,8]),  % pick one of the possible column positions
            safe( Row/Col, Rest).            % and see if that is a safe position
                                             % if not, fail back and try another column, until
                                             % the columns are all tried, when fail back to
                                             % previous row
 
safe(Anything, []).                          % the empty board is always safe
 
safe(Row/Col, [Row1/Col1 | Rest]) :-         % see if attack the queen in next row down            
            Col =\= Col1,                    % same column?
            Col1 - Col =\= Row1 - Row,       % check diagonal            
            Col1 - Col =\= Row - Row1,
            safe(Row/Col, Rest).             % no attack on next row, try the rest of board
          
member(X, [X | Tail]).                       % member will pick successive column values
 
member(X, [Head | Tail]) :-
            member(X, Tail).

board([1/C1, 2/C2, 3/C3, 4/C4, 5/C5, 6/C6, 7/C7, 8/C8]). % prototype board

run_queens(B) :- board(B),queens(B).
     
test :- get_time(Start), 
        findall( soln(B), run_queens(B), Solns ),
        get_time(End), 
        printf( "~a\n", [Solns]),
        length(Solns,Count),
        printf( "Count: ~a\n", [Count]),
        Time is End - Start,
        printf( "\nTime = ~a s\n", [Time]).
  
zebra(Owner) :- solve(S),member([_, Owner, zebra, _, _], S). 

solve(S) :- 
    S = [[C1,N1,P1,D1,S1],
         [C2,N2,P2,D2,S2],
         [C3,N3,P3,D3,S3],
         [C4,N4,P4,D4,S4],
         [C5,N5,P5,D5,S5]],
    member([red, 'English man', _, _, _], S),
    member([_, 'Swede', dog, _, _], S),
    member([_, 'Dane', _, tea, _], S),
    left_of([green |_], [white |_], S),
    member([green, _, _, coffee, _], S),
    member([_, _, birds, _, pall_mall], S),
    member([yellow, _, _, _, dunhill], S),
    D3 = milk,
    N1 = 'Norwegian',
    next_to([_, _, _, _, blend], [_, _, cats |_], S),
    next_to([_, _, _, _, dunhill], [_, _, horse |_], S),
    member([_, _, _, beer, blue_master], S),
    member([_, 'German', _, _, prince], S),
    next_to([_, 'Norwegian' |_], [blue |_], S),
    next_to([_, _, _, water,_], [_, _, _, _, blend], S),
    C1 \== C2, C1 \== C3, C1 \== C4, C1 \== C5,
    C2 \== C3, C2 \== C4, C2 \== C5,
    C3 \== C4, C3 \== C5, C4 \== C5,
    N1 \== N2, N1 \== N3, N1 \== N4, N1 \== N5,
    N2 \== N3, N2 \== N4, N2 \== N5,
    N3 \== N4, N3 \== N5, N4 \== N5,
    P1 \== P2, P1 \== P3, P1 \== P4, P1 \== P5,
    P2 \== P3, P2 \== P4, P2 \== P5,
    P3 \== P4, P3 \== P5, P4 \== P5,
    D1 \== D2, D1 \== D3, D1 \== D4, D1 \== D5,
    D2 \== D3, D2 \== D4, D2 \== D5,
    D3 \== D4, D3 \== D5, D4 \== D5,
    S1 \== S2, S1 \== S3, S1 \== S4, S1 \== S5,
    S2 \== S3, S2 \== S4, S2 \== S5,
    S3 \== S4, S3 \== S5, S4 \== S5.
 
left_of(L1, L2, [L1, L2 |_]).
left_of(L1, L2, [_| Rest ]) :- left_of(L1, L2, Rest).
    
next_to(L1, L2, S) :- left_of(L1, L2, S).
next_to(L1, L2, S) :- left_of(L2, L1, S).

test2 :- get_time(Start),
         findall( soln(B), zebra(B), Solns ),
         get_time(End),
         printf( "~a\n", [Solns]),
         length(Solns,Count),
         printf( "Count: ~a\n", [Count]),
         Time is End - Start,
         printf( "\nTime = ~a s\n", [Time]).         
