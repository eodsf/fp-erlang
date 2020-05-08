-module(second).
-export([hyp_of_right_ang_tri/2,perim_of_right_ang_tri_given_2_short_sides/2,area_of_right_ang_tri_given_2_short_sides/2]).

%a function that gives the size of the hypotenuse of a right-angled triangle given the lengths of the two other sides (pythag theorem)
hyp_of_right_ang_tri(X, Y) ->
	math:sqrt(first:square(X) + first:square(Y)).

%a function that gives the size of the perimeter of a right-angled triangle given the lengths of the two short sides
perim_of_right_ang_tri_given_2_short_sides(X, Y) ->
	Z = math:sqrt((first:square(X)) + (first:square(Y))), X + Y + Z.

%a function that gives the area of a right-angled triangle given the lengths of the two short sides	
area_of_right_ang_tri_given_2_short_sides(X, Y) ->
	first:mult(0.5, first:mult(X,Y)).