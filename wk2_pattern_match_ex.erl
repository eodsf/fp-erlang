-module(wk2_pattern_match_ex).
-export([test_suite/0]).
-define(PI, math:pi()).

%------- week 2 exercises ----------
% some refs used : 
	%https://www.analyzemath.com/Geometry_calculators/perimeter-and-area-of-triangle-given-vertices.html
	%https://www.analyzemath.com/Geometry/formulas/table_formulas_geometry.html#area_perimeter_triangle
	%https://orion.math.iastate.edu/dept/links/formulas/form2.pdf

% "Define a function perimeter/1 which takes a shape and returns the perimeter of the shape"

perimeter({circle, {_X,_Y}, R}) 
  -> 2 * ?PI * R;
perimeter({rectangle, {_X,_Y}, H, W}) 
  -> 2 * (H + W).

test_perimeter() ->
   Expected_c = ?PI * 10,
   Expected_c = perimeter({circle, {10,10}, 5}),
   18 = perimeter({rectangle, {10,10}, 3, 6}),
   passed.

% "Choose a suitable representation of triangles, and augment area/1 and perimeter/1 to handle this case too."
	%Note : I have chosen to rep. a triangle by the coords of each vertex.

perimeter_revised({circle, {_X,_Y}, R}) 
  -> 2 * ?PI * R;
perimeter_revised({rectangle, {_X,_Y}, H, W}) 
  -> 2 * (H + W);
perimeter_revised ({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}})
   -> distance_between_points({X1,Y1}, {X2,Y2}) + 
   	distance_between_points({X2,Y2}, {X3,Y3}) +
   	distance_between_points({X3,Y3}, {X1,Y1}).

distance_between_points({X1,Y1}, {X2,Y2})
   -> math:sqrt((((X2-X1)*(X2-X1)) + ((Y2-Y1)*(Y2-Y1)))).
   
test_perimeter_revised() ->
   Expected_c = ?PI * 10,
   Expected_c = perimeter_revised({circle, {10,10}, 5}),
   18 = perimeter_revised({rectangle, {10,10}, 3, 6}),
   	%note : this calculator was used for the expected result : https://www.analyzemath.com/Geometry_calculators/perimeter-and-area-of-triangle-given-vertices.html
     %for convenience, I am rounding, so instead of the calculator provided value of 15.708204, we should expect 16
     P_rounded = round(perimeter_revised({triangle, {10,10},{13,10},{10,4}})),
   16 = P_rounded,
   passed.


area({circle, {_X,_Y}, R}) 
   -> math:pi()*R*R;
area({rectangle, {_X,_Y}, H, W}) 
   -> H*W;
   %Heron's formula  : Area = sqrt [ s(s - a)(s - b)(s - c) ] , where s = (a + b + c)/2.
area({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
   %-> (X1 * (Y2 - Y3) + X2 * (Y3 - Y1) + X3 * (Y1 - Y2)) / 2.0.
      S = (perimeter_revised({triangle, {X1,Y1}, {X2,Y2},{X3,Y3}}))/2.0,
      A = distance_between_points({X1,Y1}, {X2,Y2}),
      B = distance_between_points({X2,Y2}, {X3,Y3}),
      C = distance_between_points({X3,Y3}, {X1,Y1}),
      math:sqrt(
	S * (S-A) * (S-B) * (S-C)   
      ).
   	

test_area() ->
   Expected_c = ?PI * 25,
   Expected_c = area({circle, {10,10}, 5}),
   18 = area({rectangle, {10,10}, 3, 6}),
   9.0 = area({triangle, {10,10},{13,10},{10,4}}),	%right angled triangle, base & height same as rectangle above
   passed.

% "Define a function enclose/1 that takes a shape and returns the smallest enclosing rectangle of the shape."

enclose({circle, {X,Y}, R}) 
   -> {rectangle, {X,Y}, 2*R, 2*R};
enclose({rectangle, {X,Y}, H, W}) 
   -> {rectangle, {X,Y}, H, W};				% simply the rectangle itself
enclose({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
	%https://mathopenref.com/coordbounds.html
	Max_y = lists:max([Y1,Y2,Y3]),
	Min_y = lists:min([Y1,Y2,Y3]),
	Max_x = lists:max([X1,X2,X3]),
	Min_x = lists:min([X1,X2,X3]),
	Height = Max_y - Min_y,
	Width = Max_x - Min_x,
   	{rectangle, {Min_x + (Width/2), Min_y + (Height/2)}, Height, Width}.

test_enclose() ->
   {rectangle, {35,25}, 30, 50} = enclose({rectangle, {35,25}, 30, 50}),
   {rectangle, {35,25}, 30, 30} = enclose({circle, {35,25}, 15}),
   					%point A on rect lhs, point B on top, point C on rhs
   {rectangle, {35.0,30.0}, 20, 50} = enclose({triangle, {10,20}, {35,40}, {60,40}}),
   passed.


% "Summing the bits"
% "Define a function bits/1 that takes a positive integer N and returns the sum of the bits in the binary 
% representation. For example bits(7) is 3 and bits(8) is 1."
% "See whether you can make both a direct recursive and a tail recursive definition."
% "Which do you think is better? Why?"

bits(1) -> 1;				%optimization so that we don't create a list for 1 ele
bits(N) when N>1 ->
   bits(integer_to_list(N,2), 1, 0).	%convert N to a list of base 2, giving us lists the ascii vals; are 1-indexed.

	%B is the list of ascii chars rep the binary of N; I is the index, A is an accumulator
bits(B, I, A) when I > length(B) ->
	A;				% I is passed the list len, ret result.
bits(B, I, A) ->
   bits(B, I+1, A+ (lists:nth(I,B)-48)).    %accessing the list gives us the ascii value; sub'ing 48 gets us the digit


test_bits() ->
   1 = bits(1),
   3 = bits(7),
   1 = bits(8),
   passed.
   

% --- test suite ----

test_suite() ->
   passed = test_perimeter(),
   passed = test_perimeter_revised(),
   passed = test_area(),
   passed = test_enclose(),
   passed = test_bits().