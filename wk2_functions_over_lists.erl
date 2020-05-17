-module(wk2_functions_over_lists).
-export([test_suite/0]).

% "Combining list elements: the product of a list
% Using the template from the last session, define an Erlang function to give the product of a list of numbers. 
% The product of % an empty list is usually taken to be 1: why?"

%"a "product" {\displaystyle P_{0}}P_{0} with no factors at all evaluates to 1" (https://en.wikipedia.org/wiki/Empty_product)
% --> this makes sense intuitively, because if it was 0, when we reach the base case, multiplying by 0 would result in 0!

% These are definitions in a 'direct recursion' fashion ... which clearly express the intent of the algorithm
	% first clause matches the empty list
list_product_direct([]) -> 1;
	% pattern match on head & tail, tail gets passed to the next recursion
list_product_direct([X|Xs]) -> X * list_product_direct(Xs).

% This is the same algorithm expressed in a tail recursive definition, using an accumulator param
	%list_product_tail/1 delegates to list_product_tail/2, passing an intial accumulator val of 1, 
	%representing the product of an empty list (Base case)
list_product_tail(Xs) -> list_product_tail(Xs, 1).
	%base case clause, return the accumulated value.
list_product_tail([], A) -> A;
	%arity 2; pattern match on head & tail, the accumulator is calculated afresh using the head value
list_product_tail([X|Xs], A) -> list_product_tail(Xs, X*A).
   

test_list_product() ->
   1 = list_product_direct([]),
   1 = list_product_tail([]),
   10 = list_product_direct([10]),
   10 = list_product_tail([10]),
   120 = list_product_direct([1,2,3,4,5]),
   120 = list_product_tail([1,2,3,4,5]),
   6000 = list_product_direct([10, 20, 30]),
   6000 = list_product_tail([10, 20, 30]),
   passed.   
   
% ==========================================================================================

% "Combining list elements: the maximum of a list
% Define an Erlang function to give the maximum of a list of numbers.

% You might find it helpful to use the function max/2 that gives the maximum of two values.
% It’s not obvious what should be the value for the maximum of an empty list of numbers. You could therefore choose to define 
% maximum on lists with at least one element only: to do this you will need to change the base case of the template."

% ==========================================================================================

list_max_direct([]) -> 0;
	% similar to the previous function, the direct definition is very expressive ; match on the head & tail, 
	% the eventual result is the max of the head & the result of the max of the recursion passing the tail.
list_max_direct([X|Xs]) -> max(X, list_max_direct(Xs)).


% This is the same algorithm expressed in a tail recursive definition, using an accumulator param.
	%list_max_tail/1 delegates to list_max_tail/2, passing an intial accumulator val of 0, 
	%representing the max of an empty list (Base case)
list_max_tail(Xs) -> list_max_tail(Xs, 0).

	%base case clause, return the accumulated max value.
list_max_tail([], M) -> M;
	%arity 2; pattern match on head & tail, the accumulator is calculated as max of head or the prev accumulated val.
list_max_tail([X|Xs], M) -> list_max_tail(Xs, max(X, M)).


test_list_max() ->
	0 = list_max_direct([]),
	0 = list_max_tail([]),
	10 = list_max_direct([10]),
	10 = list_max_tail([10]),
	19 = list_max_direct([7, 1, 19, 8]),
	19 = list_max_tail([7, 1, 19, 8]),
	passed.


% ==========================================================================================

test_suite() ->
	test_list_product(),
	test_list_max(),
	passed.