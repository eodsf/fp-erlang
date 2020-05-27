-module(wk4_hofs).
-export([test_suite/0]).

%
% "Using higher-order functions
% Define the functions doubleAll, evens, and product using the 
% higher-order functions lists:map, lists:filter and lists:foldr."

doubleAll([]) -> [];
doubleAll(Xs) -> lists:map(fun(X) -> X*2 end, Xs).

evens([]) -> [];
evens(Xs) -> lists:filter(fun(X) -> X rem 2 == 0 end, Xs).

product([]) -> [];
product(Xs) -> product(Xs,0).
product(Xs, A) -> lists:foldr(
			fun(X, Ac) -> Ac+(X*X) end, 
		    A, Xs).


%
%"Zipping
% a) Define a function zip/2 that zips together pairs of elements from two lists 
% like this:
% zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]
% where you can see that the elements from the longer list are lost.

% this could prob have been done with mapfoldl or similar ... 

zip(X,Y) 	 -> lists:reverse(
			zip(X,Y,[])
		     ).
		     
zip([], _Y, A) 			 -> A;		%Xs is exhausted 
zip(_X, [], A) 			 -> A;		%Y is also exhausted
zip([X|Xs], [Y|Ys], A) ->
   %create the new term from the head of each list & accumulate it
   Acc = [{X, Y} | A],   
   	zip(Xs, Ys, Acc).				% tail recurse.
   	

% "b) Define a function zip_with/3 that zips together pairs of elements from two 
%	lists using the function in the first argument, like this:
%	zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]"

% eg, zip_with creates a new list which is the sum of each ele pulled from 
% each 'fold' over Xs & Ys
zip_with(F, X, Y)		-> lists:reverse(zip_with(F,X,Y,[])).
zip_with(_F, [], _Y, A)		-> A;
zip_with(_F, _X, [], A)		-> A;

zip_with(F, [X|Xs], [Y|Ys], A) ->
   %apply the provided Fun to X & Y, and accumulate 
   Acc = [F(X,Y) | A],
   zip_with(F, Xs, Ys, Acc).
	
	
% "c) Re-define the function zip_with/3 using zip and lists:map."
%Hmn, not sure if we were supposed to pass zip in as a param ? I call it inline,
%which result in arity /2
zip_with_redefined(X, Y) 	-> 
   Zipped = zip(X,Y),
   % Zipped holds the tuples from both lists, we just need to sum them
   % Zipped is passed as the list arg to map, and Fun sums the tuple vals
   % if Zipped is [] at this point it should be handled gracefully
   
	%note : vars pass to anon fun renamed to avoid 'shadowed' warning
	%finding this was helpful:
	%https://stackoverflow.com/questions/27272765/executing-a-function-on-a-list-and-a-list-of-lists-erlang
   lists:map( fun({X1,Y1}) -> X1+Y1 end,	
   		Zipped).
   		
% "d) Re-define zip/2 using zip_with/3."

   

doubleAll_provided([]) -> [];
doubleAll_provided([X|Xs]) ->
    [ 2*X | doubleAll_provided(Xs) ].

evens_provided([]) -> [];
evens_provided([X|Xs]) when X rem 2 == 0 ->
    [X | evens_provided(Xs) ];
evens_provided([_|Xs]) ->
    evens_provided(Xs).

product_provided([]) -> 1;
product_provided([X|Xs]) -> X * product_provided(Xs).


test_evens() ->
   [2,4,6,8] = evens([1,2,3,4,5,6,7,8]),
   pass.
   
test_doubleAll() ->
   [8,16,24,32] = doubleAll([4,8,12,16]),
   pass.
   
test_product() ->
    120 = product([2,4,6,8]),
    pass.
    
test_zip() ->
   [{1,2}, {3,4}] = zip([1,3,5,7], [2,4]),
   pass.
   
test_zip_with() ->
   [3,7] = zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]),
   pass.

test_zip_with_redefined() ->
   [3,7] = zip_with_redefined([1,3,5,7], [2,4]),
   pass.
   		

 test_suite() ->
    pass = test_evens(),
    pass = test_doubleAll(),
    pass = test_product(),
    pass = test_zip(),
    pass = test_zip_with(),
    pass = test_zip_with_redefined(),  
    pass.