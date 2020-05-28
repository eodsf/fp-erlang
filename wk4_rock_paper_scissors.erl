-module(wk4_rock_paper_scissors).
-export([test_suite/0]).

% Given a valid move, returns the move that beats it
beat(rock) 	-> paper;
beat(paper) 	-> scissors;
beat(scissors)  -> rock.

% Given a valid move, returns the move that it beats.
lose(rock) 	-> scissors;
lose(paper) 	-> rock;
lose(scissors)  -> paper.

% Returns the result of a 'turn', defined by the order of the moves provided,
% and from the point of view of the first; eg, result(rock,paper) = lose
%
% Valid results are : win, lose, draw.
result(MoveA, MoveB) when (MoveA == MoveB) 		-> draw;

result(MoveA, MoveB)  	-> 
   case (beat(MoveA) == MoveB) of
	true ->	lose;
	_    -> win
   end.
   
% Gives the result of a series of rounds, each series of rounds provided as a list.
% The result is an integer, being the difference of the wins for player A and
% player B (player A's move is always provided as the first move in a round).
% The result is from the perspective of player A - a +ive integer means a win for
% player A, a -ive integer means a loss (a win for player B), 0 means a draw.
% For now, we assume 2 lists only as input (q : is there a way to represent
% a variable # of args?)

tournament(X,Y) 	-> tournament(X,Y,[]).  

% both lists are exhausted; any other secnario means a list of uneven size
% was provided (bad input) & probably should let it crash.
% now we can just reduce the accumulated results
tournament([],[], R) 	-> lists:foldl(fun(X, Sum) -> X + Sum end, 0, R);

%accumulate the result of every round
tournament([X|Xs],[Y|Ys], A) ->    
   tournament(Xs,Ys, [convert_result_to_int(result(X,Y)) | A]).

% re-implementation of the above using only lists: funcs ! very compact !
tournament_better(Xs, Ys) ->
   Zipped = lists:zipwith(fun(X, Y) -> convert_result_to_int(result(X,Y)) end, 
   			Xs, Ys),
   lists:foldl(fun(X, Sum) -> X + Sum end, 0, Zipped).


% convenience function to convert result atom to an int
convert_result_to_int(win) -> 1;
convert_result_to_int(lose) -> -1;
convert_result_to_int(draw) -> 0.
   
test_tournament() ->
   0 = tournament([rock,paper], [rock,paper]),
   2 = tournament([paper,scissors], [rock,paper]),
   -2 = tournament([rock,scissors], [paper,rock]),   
   pass.

test_tournament_better() ->
   0 = tournament_better([rock,paper], [rock,paper]),
   2 = tournament_better([paper,scissors], [rock,paper]),
   -2 = tournament_better([rock,scissors], [paper,rock]),   
   pass.
   
test_convert_result_to_int() ->
   1 = convert_result_to_int(win),
   0 = convert_result_to_int(draw),
   -1 = convert_result_to_int(lose),
   pass.



test_suite() ->
   draw = result(rock, rock),
   draw = result(paper, paper),
   draw = result(scissors, scissors), 
   lose = result(rock, paper),
   lose = result(paper, scissors),
   lose = result(scissors, rock),
   win = result(rock, scissors),
   win = result(scissors, paper),
   win = result(paper, rock),
   pass = test_tournament(),
   pass = test_tournament_better(),
   pass.
