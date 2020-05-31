-module(wk4_rps_strategies).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2]).


%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament
play_two(_,_,PlaysL,PlaysR,0) ->
   io:format("Tournament result: ~p ~n", [tournament(PlaysL, PlaysR)]);

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   play_two(StrategyL, StrategyR, 
   	[StrategyL(PlaysL) | PlaysL], 
   	[StrategyR(PlaysR) | PlaysR], N - 1).

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");

	_    ->
	    Result = result(Play,Strategy(Moves)),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

%
% strategies.
%
% A strategy is a function from the list of opponent’s plays 
% (latest at the head) to a choice of play for the next turn

echo([]) ->
     paper;
echo([Last|_]) ->
   %this at least assures you will 'draw' with them for the round
    Last.

rock(_) ->
    rock.



% "Assume that your opponent never repeats herself: if you know this you can make a 
% choice that will never lose"

no_repeat([]) ->
   % going w/ the same strategy as Simon does here for echo
    paper;

% Since we just want to beat the opponent for this round, I don't think it's necessary
% to deconstruct their move history (eg, [Last|Prev|_]) - we just need to 
% ensure we can beat their last played move    
no_repeat([X|_]) ->   
    beat(X).

% There was some confusion of the purpose of this, but it seems that
% its purpose is to just return the same strategy regardless of argument passed.

const(Play) ->
   fun(_) -> Play end.

% cycles through the three choices in some order
% just cycle through the next in the 'r,p,s' seq depending on the last play
% this could be more concise if we used the enum values to effect
cycle([]) 		-> rock;
cycle([rock | _]) 	-> paper;
cycle([paper | _]) 	-> scissors;
cycle([scissors | _]) 	-> rock.

% make a random choice each time
rand(_) ->
    enum(rand:uniform(3)-1).

% Given a valid move, returns the move that beats it
beat(rock) 	-> paper;
beat(paper) 	-> scissors;
beat(scissors)  -> rock.