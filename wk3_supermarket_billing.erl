%% coding: latin-1

-module(wk3_supermarket_billing).
-export([produce_bill/1, update_inventory/1]).
-define(TOTAL, total).

%"A supermarket billing system will take a sequence of barcodes such as
%[1234,4719,3814,1112,1113,1234]
%into a printed bill"
%The aim of this exercise is to define the function that will produce the bill 
%from a list of barcodes and the database.
%To take the problem further you might like to add these features:
% - You are asked to add a discount for multiple buys of sherry: for every two 
%	bottles bought, there is a £1.00 discount.
% - Design functions which update the database of bar codes. You will need a 
%	function to add new information while removing any entry for the same 
%	bar code.
% - Re-design your system so that bar codes which do not appear in the database 
%	{e.g. 1113 in the example} give no entry in the final bill

% Given a list of barcodes, produce a list of strings representing lines on a bill

produce_bill([]) -> [];
produce_bill(X) -> 
   Db = init(),
   LineItems = calculate_line_item_totals(X, #{}, Db),
   io:format("LineItems ~p ~n",[LineItems]),
   print_bill(LineItems, Db).
      
% Prints the bill to std. out
print_bill(M, Db) ->	
      io:format("Erlang Stores~n~n"),      

      %fold over the calculated line items (barcode -> line item total)
      maps:fold(
      		fun(K, V, ok) ->
      			%get the item desc from the inventory Db by the barcode key
      			{I, _P} = maps:get(K, Db),
      			io:format("~p ~p ~n", [I, V])
		end, ok, M),
 	print_total(M).
 	
 % Sum & print the total from the line item totals	
 print_total(M) ->
    io:format("Total : ~p~n",[lists:sum(maps:values(M))]).

% Calculates the line item totals, collecting in the map aggregator (barcode -> total)
calculate_line_item_totals([], M, _Db) -> M;
calculate_line_item_totals([X|Xs], M, Db) ->
	io:format("Db : ~p~n",[Db]),
   UpdatedMap = calculate_line_item_total(X, M, Db),
   io:format("UpdatedMap : ~p~n",[UpdatedMap]),
   calculate_line_item_totals(Xs, UpdatedMap, Db).

% Using the barcode (B) and the Db of inventory, populate the map M with 
% an (updated) tally of barcode to total per item; ignores any barcodes passed
% that are not inventoried.
calculate_line_item_total(B, M, Db) ->
   case maps:is_key(B, Db) of
      true -> {_I, P} = maps:get(B, Db),       		
    		maps:update_with(B, fun(V) -> V+P end, P, M);
       
      _-> M
   end.


% init the system with a sample DB
init() ->
  % The sample DB is described as a list of tuples
   Db = 
   [{4719, "Fish Fingers" , 121},
    {5643, "Nappies" , 1010},
    {3814, "Orange Jelly", 56},
    {1111, "Hula Hoops", 21},
    {1112, "Hula Hoops {Giant}", 133},
    {1234, "Dry Sherry, 1lt", 540}],
   init_inventory(Db).
	
   
   % produce a map of barcode => {item, price} for use in the system
   -spec init_inventory([{integer(), string(), integer()}]) ->
   						#{integer() => {string(), integer()}}.
   init_inventory([]) -> #{};
   init_inventory(Db) -> update_inventory(Db).
   
% Update the inventory database given a list of tuples of the form
% {barcode, item name, price in pennies}
update_inventory([]) -> #{};
update_inventory(X) -> 
   update_inventory(X,#{}).
update_inventory([], M) -> M;   
update_inventory([{B,I,P}|Xs], M) -> 
   UpdatedMap = update_inventory_item(B, {I, P}, M),
   update_inventory(Xs, UpdatedMap).
    
update_inventory_item(B, IP, M) ->
   maps:update_with(B, fun(_V) ->
   			IP
   			end,
   		IP, M).    

test_update_inventory() ->
   #{4719 := {"Fish Fingers",121}} = update_inventory([{4719, "Fish Fingers" ,121}]),
pass.   		
   		
test_update_inventory_item() ->
   #{4719 := {"Fish Fingers",121}} = update_inventory_item(4719, {"Fish Fingers" ,121}, #{}),
pass.
   		
test_init() ->
   Map = init(),
   {"Fish Fingers",121} = maps:get(4719, Map),
   {"Nappies" , 1010} = maps:get(5643, Map),
    {"Orange Jelly", 56} = maps:get(3814, Map),
    {"Hula Hoops", 21} = maps:get(1111, Map),
    {"Hula Hoops {Giant}", 133} = maps:get(1112, Map),
    {"Dry Sherry, 1lt", 540} = maps:get(1234, Map),
    pass.