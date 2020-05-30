-module(wk4_funs_as_results).
-export([test_suite/0]).

% Define a function that takes a list of functions and composes them together.

% compose/2 returns a partially applied function on X; each ele in the list
% is successively passed to compose along with a second function - 
% on the FIRST application this is the identity function - but the partial 
% application returned by compose is a new function ( F1(X), since id(X) returns X),
% which is the new accumulator - passed as the 2nd arg to compose in the next
% 'iteration' - which results in F2(F1(X)) being returned, and so on !
% resulting in the partial application
list_composer(Xs) ->
    lists:foldl(fun compose/2, fun id/1, Xs).	   
    
    
test_list_composer() ->	   
   Composed = list_composer([fun(X) -> X+1 end, fun(X) -> X+2 end]),
   13 = Composed(10),
   pass.

   
   
%"Using compose or otherwise, define a function twice that applies a function to an 
%argument twice"   

twice(F) ->
   compose(F,F).   
   
test_twice() ->
    F = twice(fun(X) -> X*3 end), 
    18 = F(2),
    pass.
   
   
% "What happens when you apply twice to itself? 
% What happens when you apply the result of that to “multiply by 3” and the 
% result of that to 2?   "

test_twice_calling_itself() ->
    F = twice(fun twice/1),
    F2 = F(2),
    4 = F2(2),
    pass.
      
   
 % "Define a function iterate that takes a number N and returns a function that 
 % takes a function and returns that function iterated N times. 
 % When N is zero, it should return the identity function 
 % (that is, the function that returns its argument unchanged).   
 
 iterate(N) when (N==0) -> 
 		%returns a function that applies the identity function to its arg
 		fun(X) -> id(X) end;
 iterate(N) -> 	
    fun(F) -> 
       	%apply the function each times in succession for a total of N times
       	lists:foldl( fun(X) -> F(X) end, 0, lists:seq(1,N))    	   
    end.
 
 
 test_iterate() ->
    FId = iterate(0),
    1 = FId(1),		%application of identity function obtained from iterate
    
    F = iterate(3),
    F2 = F( fun(X) -> X+1 end ), %applying a simple addition of 1 as param
    6 = F2(3),			 % application 3 times of our addition on param 3
    pass.
 
   
% provided function - takes 2 function args, and returns a function (partial
% application), that when called/applied composes the originally supplied functions
% to its single arg X
compose(F,G) ->
    fun(X) -> G(F(X)) end.   
    
% provided function    
add(X) ->
    fun(Y) -> X+Y end.

% provided function
times(X) ->
    fun(Y) ->
	     X*Y end.
	     	     
% provided function - the identity function
id(X) ->
    X.

    
test_suite() ->
   pass = test_twice(),
   pass = test_list_composer(),
   %pass = test_twice_calling_itself(),
   test_iterate(),
   pass.