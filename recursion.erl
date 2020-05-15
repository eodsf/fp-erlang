-module(recursion).
-export([test_suite/0]).

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
   
   
% "The function fib/1 that we defined earlier is exponentially complex … ouch! 
% Define an efficient Fibonacci function fib/3 using a tail recursion with two accumulating parameters 
% that hold the last two Fibonacci numbers. 
% Give a step-by-step evaluation of fib(4)."

% hand-evaluation of the following function :
%
%      fib_tail_recursion(4)
%   == fib_tail_recursion(4, 0, 1)
%   == fib_tail_recursion(3, 0+1 == 1, 0)
%   == fib_tail_recursion(2, 1+0 == 1, 1)
%   == fib_tail_recursion(1, 1+1 == 2, 1)
%   == fib_tail_recursion(0, 2 + 1 ==3, 1)
%   		==> (pattern match on 2nd param 'X' to return its value) == 3

% Notes : 
% func/1 calls the function w/ the first two 'known' values from which
% the rest of the sequence can be calculateed; this is similar to a bottom-up DP algo.
% (e.g., 'X' == value of N-2 & 'Y' == value of N-1 in the intial call)
con
% in the recursion, as we accumulate in 'X', 'Y' tracks the old value of X
% it's helpful to think of this as a loop, we're just iterating backwards over N, but 
% otherwise this is exactly what a bottom-up DP algorithm optimized for space would do
% (iterate 1..N & store the last 2 calculations)
% eg : https://dev.to/rattanakchea/dynamic-programming-in-plain-english-using-fibonacci-as-an-example-37m1

fib_tail_recursion(N) -> fib_tail_recursion(N, 0, 1).

fib_tail_recursion(0, X, _) -> X;	    %base case : return the accumulated val
fib_tail_recursion(N, X, Y) when N>0 ->
	fib_tail_recursion(N-1, X+Y, X).    
   
   
test_fib_tail_recursion() ->
  0 = fib_tail_recursion(0),
  1 = fib_tail_recursion(1),
  1 = fib_tail_recursion(2),
  2 = fib_tail_recursion(3),
  3 = fib_tail_recursion(4),
  5 = fib_tail_recursion(5),
  8 = fib_tail_recursion(6),
  13 = fib_tail_recursion(7),
  21 = fib_tail_recursion(8),
  passed.


% "Perfect numbers"
% "A positive integer is perfect when it is the sum of its divisors, 
% e.g. 6=1+2+3, 28=1+2+4+7+14.
% Define a function perfect/1 that takes a positive number N and returns a 
% boolean which indicates whether or not the number is perfect. 
% You may well want to use an accumulating parameter to hold the sum of the divisors 
% “so far”.


perfect(N) -> perfect(N, N, 0).

	%base case : does N_ORIG == the accumulated value ACC ?
perfect(0, N_ORIG, ACC) -> io:format("N=~p : ACC=~p~n", [N_ORIG,ACC]), 
				N_ORIG == ACC;

% a case statement & helper function is used to check if a divisor but not N itself; inc ACC only in this case
perfect(N, N_ORIG, ACC) when N >= 1 ->
	perfect(N-1, N_ORIG, 
		case 
		   is_divisor_of_N_but_not_N(N_ORIG, N) of
			true -> ACC + N;
			false -> ACC
		end).					

is_divisor_of_N_but_not_N(N, N)	-> false;
is_divisor_of_N_but_not_N(N, X) -> N rem X == 0.

	
test_perfect() ->
   true = perfect(6),
   true = perfect(28),
   passed.
   
   
% ---------- OVERALL TEST SUITE ---------------


test_suite() ->
  passed = test_fib(),
  passed = test_pieces(),
  passed = test_fib_tail_recursion(),
  passed = test_perfect().
