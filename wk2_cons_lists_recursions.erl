-module(wk2_cons_lists_recursions).
-export([test_suite/0]).

% "The aim of these exercises is to familiarise you with other ways of defining functions over 
% lists in Erlang, in particular the different way that recursive functions can construct lists."

% "Define an Erlang function double/1 to double the elements of a list of numbers."

double([]) -> [];
double([X|Xs]) ->
   [X*2 | double(Xs)].

test_double() ->
   [2,10,16,20] = double([1,5,8,10]),
   passed.



% "Define a function evens/1 that extracts the even numbers from a list of integers."

is_even(X) when X >= 0 -> (X band 1) == 0.

evens([]) -> [];
evens([X|Xs]) ->
		case (is_even(X)) of
      true -> evens([X | Xs]);		
			_-> evens(Xs)
		end.
	
test_evens() ->
   [] = evens([]),
   [] = evens([0]),
   [2,4,6,8] = evens([1,2,3,4,5,6,9,7,8]),
   passed.

% "the median of a list of numbers: this is the middle element when the list is ordered 
% (if the list is of even length you should average the middle two)"


% "the modes of a list of numbers: this is a list consisting of the numbers that occur most 
% frequently in the list; if there is is just one, this will be a list with one element only"

test_suite() ->
   test_double(),
   test_evens(),
   passed.