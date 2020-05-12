-module(recursion).
-export([test_suite/0]).

%as an aside, see this gist for a far more performant tail-recursive method:
% https://gist.github.com/gorkaio/1cf3c12af3ec69265a1029c7d8e1cb25
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 ->
  fib(N-1) + fib(N-2).


test_fib() ->
  0 = fib(0),
  1 = fib(1),
  1 = fib(2),
  2 = fib(3),
  3 = fib(4),
  5 = fib(5),
  8 = fib(6),
  13 = fib(7),
  21 = fib(8),
  passed.

  
% Step-by-step evaluation of fib(4):
% fib(4)
% = fib(3) + fib(2)
% = (fib(2) + fib(1)) + (fib(1) + fib(0))
% = ((fib(1) + fib(0)) + fib(1)) + (1 + 0)
% = (1 + 0 + 1) + (1 + 0)
% = 3


% Define a function pieces so that pieces(N) tells you the maximum number of pieces into 
% which you can cut a piece of paper with N straight line cuts.
%Assumption : the paper is square or rectangular ;-)

% note : someone on the course ref'd this as the inspiration for this question : http://www.jlmartin.faculty.ku.edu/MiniCollege2012/handout.pdf

% The max # of pieces depends on how many 'spaces' in the overall paper the new line can bisect
% pieces(0) == 1 (no cut, original paper (piece) intact)
% pieces(1) == 2 (bisect original into 2 planes)
% pieces(2) == 4 (bisecting the 2 planes, resulting in 4 pieces)
% pieces(3) == 7 (bisecting 3 planes, resulting in 7 pieces) 
% pieces(4) == 11 (bisecting 4 planes) -> 7 + 4
% -> we see a possible formula : result of prev + # planes we are able to bisect; also, the # of planes we can bisect seems to inc by 1 with
% each iteration

%'base case'
pieces(0) -> 1;

pieces(N) when N >0 -> 
 pieces(N-1) + N.

test_pieces() ->
   1 = pieces(0),
   2 = pieces(1),
   4 = pieces(2),
   7 = pieces(3),
   11 = pieces(4),
   passed.

test_suite() ->
  passed = test_fib(),
  passed = test_pieces().
