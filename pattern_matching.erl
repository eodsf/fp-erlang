%------------------------------------------------------------------------------
%Some general notes on pattern matching:
%Clauses are matched sequentially, separeted by ';' - prefer more specific to more vague for best matching.
%One example given, for defining an XOR function, was:

%xOr(true,false) ->
%true;
%xOr(false,true) ->
%true
%xOr(X,Y) ->
%false.

%An alternative, more concise definition, with a pattern match that matches anything in the last clause:

%xOr(X,X) ->
%false;
%xOr(_,_) ->
%false;

%The exercise for this part of the course is to write functions using pattern matching where possible.
%------------------------------------------------------------------------------

%One very useful tip on test implementation (from moderator Brujo Benavides) is to use pattern matching :
%"You can use pattern-matching for tests, too… instead of equality. i.e. you can write `0 = how_many_equal(34, 25, 36)` %instead of using `==` and then the test function will crash if you have an error instead of returning false."


-module(pattern_matching).
-export([test_suite/0]).

%0.
%In the previous video step on pattern matching we saw two ways of defining “exclusive or”. Give at least three others. You %might find it useful to know that:
%=/= and == are the operations for inequality and equality in Erlang;
%not is the Erlang negation function; and,
%and and or are the Erlang conjunction and disjunction (infix) operators.

xOr_alt1(X,Y) ->
	X =/= Y.
	

xOr_alt2(X,Y) ->
	not (X==Y).


xOr_alt3(X,Y) ->
	(X and not Y) or (Y and not X).


test_xOr() ->
	true = xOr_alt1(false, true),
	true = xOr_alt1(true, false),
	false = xOr_alt1(false, false),
	false = xOr_alt1(true, true),
	
	true = xOr_alt2(false, true),
	true = xOr_alt2(true, false),
	false = xOr_alt2(false, false),
	false = xOr_alt2(true, true),
	
	true = xOr_alt3(false, true),
	true = xOr_alt3(true, false),
	false = xOr_alt3(false, false),
	false = xOr_alt3(true, true),
	passed.


%1.

%Give a definition of the function maxThree which takes three integers and returns the maximum of the three. You can use the %max function, which gives the maximum of two numbers, in writing your definition.

max_three(X,Y,Z) ->
	max(max(X,Y),Z).
	

test_max_three() ->
	1 = max_three(1, 0, -1),
	10 = max_three(2, 7, 10),
	99 = max_three(2, 99, 98),
	passed.


%2.
%Give a definition of the function howManyEqual which takes three integers and returns an integer, counting how many of its %three arguments are equal.
how_many_equal(X, X, Z) -> 2;
how_many_equal(X, Y, Y) -> 2;
how_many_equal(Z, Y, Z) -> 2;
how_many_equal(X, X, X) -> 3;
how_many_equal(_, _, _) -> 0.
	

test_how_many_equal() ->
     0 = how_many_equal(34,25,36),
	 2 = how_many_equal(34,25,34),
	 3 = how_many_equal(34,34,34),
	 passed.

%---------- overall test suite -------------	 
	 
test_suite() ->
	passed = test_xOr(),
	passed = test_max_three(),
	passed = test_how_many_equal().	 