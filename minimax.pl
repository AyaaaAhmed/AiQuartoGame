





% ------------------------------------------------------------------------------------------





:- use_module(minimax).


% bestMove(+Pos, -NextPos)

% Compute the best Next Position from Position Pos

% with minimax or alpha-beta algorithm.

bestMove(Pos, NextPos) :-
    
minimax(Pos, NextPos, _).
piece(1,'Black&','Round&','Tall').
piece(2,'Black&','Round&','Square').
piece(3,'Black&','Square&','Tall').
piece(4,'Black&','Square&','Square').
piece(5,'White&','Round&','Tall').
piece(6,'White&','Round&','Square').
piece(7,'White&','Square&','Tall').
piece(7,'White&','Square&','Tall').
piece(8,'White&','Square&','Square').
piece(0,' ',' ',' ').


printNextBoard(Board,9):-!.
printNextBoard(Board,X):-
\+member(X,Board), piece(X,A,B,C), write(A), write(B), write(C), write(','), nl, X1 is X+1,
printNextBoard(Board,X1). 
printNextBoard(Board,Y):-
member(Y,Board), Y1 is Y+1,
printNextBoard(Board,Y1). 



% play

% Start the game.
play :-
  nl,
    write('==================='), 
  nl,    write('== Prolog Quarto =='),
  nl,
    write('==================='), 
  nl, nl, nl,
    playAskColor.
	
	
	


% playAskColor

% Ask the color for the human player and start the game with it.

playAskColor :-
%************************btghz el board el fady
	  nl,nl,nl,

	  ( !,     
% If not x or o -> not a valid color                   
% Ask again
	    EmptyBoard = [0, 0, 0, 0, 0, 0, 0, 0, 0],
	   % nb_linkval(pieces,EmptyBoard)
	    show(EmptyBoard), nl,
	

% Start the game with color and emptyBoard
	    play([x, play, EmptyBoard], Player)
	  ).



% play(+Position, +HumanPlayer)

% If next player to play in position is equal to HumanPlayer -> Human must play

% Ask to human what to do.

play([Player, play, Board], Player) :- 
  !,printNextBoard(Board,1),nl,
 nl,write('Enter The ID of Piece : ') ,read(Id),
  write('Next move ?'), 
  nl, read(Pos), nl,
% Ask human where to play
    (
      humanMove([Player, play, Board], [NextPlayer, State, NextBoard], Pos,Id), !,
      show(NextBoard),
      (
        State = win, !,                             
% If Player win -> stop
        nl, write('End of game : '),
        write(Player), write(' win !'), nl, nl
        ;
 State = draw, !,                            
% If draw -> stop
        nl, write('End of game : '),
        write(' draw !'), nl, nl
        ;
        play([NextPlayer, play, NextBoard], Player) 
% Else -> continue the game
      )
 ;
write('-> Bad Move !'), nl,                
% If humanMove fail -> bad move
      play([Player, play, Board], Player)        
% Ask again
    ).




% play(+Position, +HumanPlayer)

% If it is not human who must play -> Computer must play

% Compute the best move for computer with minimax or alpha-beta.

play([Player, play, Board], HumanPlayer) :-
    
  nl, write('Computer play : '), nl, nl,
    

% Compute the best move
    bestMove([Player, play, Board], [NextPlayer, State, BestSuccBoard]),
    show(BestSuccBoard),
    (
      State = win, !,                                 
% If Player win -> stop
      nl, write('End of game : '),
      write(Player), write(' win !'), nl, nl
      ;
      State = draw, !,                                
% If draw -> stop
      nl, write('End of game : '), write(' draw !'), nl, nl
      ;
      
% Else -> continue the game
      play([NextPlayer, play, BestSuccBoard], HumanPlayer)
    ).

% nextPlayer(X1, X2)

% True if X2 is the next player to play after X1.

nextPlayer(o, x).

nextPlayer(x, o).



% When human play
humanMove([X1, play, Board], [X2, State, NextBoard], Pos,Id) :-
    nextPlayer(X1, X2),
    set1(Pos, Id, Board, NextBoard),
    (
      winPos(NextBoard), !, State = win ;
      drawPos(NextBoard), !, State = draw ;
      State = play
    ).




% set1(+Elem, +Pos, +List, -ResList).

% Set Elem at Position Pos in List => Result in ResList.

% Rem : counting starts at 1.

set1(1, E, [X|Ls], [E|Ls]) :- 
  !, X = 0.


set1(P, E, [X|Ls], [X|L2s]) :-
    
  number(P),
    
  P1 is P - 1,
    
  set1(P1, E, Ls, L2s).




% show(+Board)

% Show the board to current output.

show([X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
  write('   '), show2(X1),
  ( X1 = 0 -> write('                 | '); write('  | ') ), show2(X2),
 ( X2 = 0 -> write('                   | '); write('  | ') ), show2(X3), nl,
  write('  ------------------------------------------------------------'), nl,
  write('   '), show2(X4),
 ( X4 = 0 -> write('                 | '); write('  | ') ), show2(X5),
 ( X5 = 0 -> write('                   | '); write('  | ') ), show2(X6), nl,
  write('  ------------------------------------------------------------'), nl,
  write('   '), show2(X7), 
  ( X7 = 0 -> write('                 | '); write('  | ') ), show2(X8),
 ( X8 = 0 -> write('                   | '); write('  | ') ), show2(X9), nl.





% show2(+Term)
% Write the term to current outupt
% Replace 0 by ' '.
show2(X) :-
  X = 0, !,
    write(' ').


show2(X) :-
  piece(X,A,B,C),
  
  write(A),write(B),write(C).



:- module(minimax, [minimax/3]).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Pos, BestNextPos, Val) :-                     % Pos has successors
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    best(NextPosList, BestNextPos, Val), !.

minimax(Pos, _, Val) :-                     % Pos has no successors
    utility(Pos, Val).


best([Pos], Pos, Val) :-
    minimax(Pos, _, Val), !.

best([Pos1 | PosList], BestPos, BestVal) :-
    minimax(Pos1, _, Val1),
    best(PosList, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).



betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0



% ------------------------------------------------------------------------------------------
:- module(tictactoe, [move/2,min_to_move/1,max_to_move/1,utility/2,winPos/2,drawPos/2]).


% move(+Pos, -NextPos)

% True if there is a legal (according to rules) move from Pos to NextPos.


move([X1, play, Board],[X2, win, NextBoard]) :-

 nextPlayer(X1, X2),

 ((move_aux(1, Board, NextBoard),\+member(1,Board));
 (move_aux(2, Board, NextBoard),\+member(2,Board));
 (move_aux(3, Board, NextBoard),\+member(3,Board));
 (move_aux(4, Board, NextBoard),\+member(4,Board));
 (move_aux(5, Board, NextBoard),\+member(5,Board));
 (move_aux(6, Board, NextBoard),\+member(6,Board));
 (move_aux(7, Board, NextBoard),\+member(7,Board));
 (move_aux(8, Board, NextBoard),\+member(8,Board)))
 ,winPos(NextBoard), !.

nextPlayer(x, o).
nextPlayer(o, x).


move([X1, play, Board],[X2, draw, NextBoard]) :-
 nextPlayer(X1, X2),
((move_aux(1, Board, NextBoard),\+member(1,Board));
 (move_aux(2, Board, NextBoard),\+member(2,Board));
 (move_aux(3, Board, NextBoard),\+member(3,Board));
 (move_aux(4, Board, NextBoard),\+member(4,Board));
 (move_aux(5, Board, NextBoard),\+member(5,Board));
 (move_aux(6, Board, NextBoard),\+member(6,Board));
 (move_aux(7, Board, NextBoard),\+member(7,Board));
 (move_aux(8, Board, NextBoard),\+member(8,Board))),
 drawPos(NextBoard),!.



move([X1, play, Board], [X2, play, NextBoard]) :-
 nextPlayer(X1, X2),
  ((move_aux(1, Board, NextBoard),\+member(1,Board));
 (move_aux(2, Board, NextBoard),\+member(2,Board));
 (move_aux(3, Board, NextBoard),\+member(3,Board));
 (move_aux(4, Board, NextBoard),\+member(4,Board));
 (move_aux(5, Board, NextBoard),\+member(5,Board));
 (move_aux(6, Board, NextBoard),\+member(6,Board));
 (move_aux(7, Board, NextBoard),\+member(7,Board));
 (move_aux(8, Board, NextBoard),\+member(8,Board))),!.



% move_aux(+Player, +Board, -NextBoard)

% True if NextBoard is Board whith an empty case replaced by Player mark.


move_aux(P, [0|Bs], [P|Bs]).


move_aux(P, [B|Bs], [B|B2s]) :-
    
move_aux(P, Bs, B2s).




% min_to_move(+Pos)

% True if the next player to play is the MIN player.

min_to_move([o, _, _]).


% max_to_move(+Pos)

% True if the next player to play is the MAX player.

max_to_move([x, _, _]).


% utility(+Pos, -Val) :-

% True if Val the the result of the evaluation function at Pos.

% We will only evaluate for final position.

% So we will only have MAX win, MIN win or draw.

% We will use  1 when MAX win

%             -1 when MIN win

%              0 otherwise.

utility([o, win, _], 1).       
% Previous player (MAX) has win.

utility([x, win, _], -1).      
% Previous player (MIN) has win.

utility([_, draw, _], 0).


% winPos(+Player, +Board)

% True if Player win in Board.


winPos([X1, X2, X3, X4, X5, X6, X7, X8, X9]) :-
(piece(X1,A1,B1,C1),
piece(X2,A2,B2,C2),
piece(X3,A3,B3,C3),
piece(X4,A4,B4,C4),
piece(X5,A5,B5,C5),
piece(X6,A6,B6,C6),
piece(X7,A7,B7,C7),
piece(X8,A8,B8,C8),
piece(X9,A9,B9,C9)),
((equal(A1, A2, A3),A1\=' ') ;    
% 1st line
    
(equal(A4, A5, A6),A4\=' ') ;    
% 2nd line
    
(equal(A7, A8, A9),A7\=' ') ;    
% 3rd line
    
(equal(A1, A4, A7),A1\=' ') ;    
% 1st col
    
(equal(A2, A5, A8),A2\=' ') ;    
% 2nd col
    
(equal(A3, A6, A9),A3\=' ') ;
% 3rd col
    
(equal(A1, A5, A9),A1\=' ') ;    
% 1st diag
    
(equal(A3, A5, A7),A3\=' ') ;

% 2nd diag
(equal(B1, B2, B3),(B1\=' ')) ;    
% 1st line
    
(equal(B4, B5, B6),B4\=' ') ;    
% 2nd line
    
(equal(B7, B8, B9),B7\=' ') ;    
% 3rd line
    
(equal(B1, B4, B7),B1\=' ') ;    
% 1st col
    
(equal(B2, B5, B8),B2\=' ') ;    
% 2nd col
    
(equal(B3, B6, B9),B3\=' ') ;
% 3rd col
    
(equal(B1, B5, B9),B1\=' ') ;    
% 1st diag
    
(equal(B3, B5, B7),B3\=' ');

% 2nd diag
(equal(C1, C2, C3),C1\=' ') ;    
% 1st line
    
(equal(C4, C5, C6),C4\=' ') ;    
% 2nd line
    
(equal(C7, C8, C9),C7\=' ') ;    
% 3rd line
    
(equal(C1, C4, C7),C1\=' ') ;    
% 1st col
    
(equal(C2, C5, C8),C2\=' ') ;    
% 2nd col
    
(equal(C3, C6, C9),C3\=' ') ;
% 3rd col
    
(equal(C1, C5, C9),C1\=' ') ;    
% 1st diag
    
(equal(C3, C5,C7),C3\=' ')

).     
% 2nd diag



% drawPos(+Player, +Board)

% True if the game is a draw.

drawPos(Board) :-
    
  count(Board,0,X),
  X=:=1.




% equal(+W, +X, +Y, +Z).

% True if W = X = Y = Z.

equal(X, X, X).

count([],X,0):-!.
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z,!.
count([X1|T],X,Z):- X1\=X,count(T,X,Z),!.

