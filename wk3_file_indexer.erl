%% coding: latin-1
-module(wk3_file_indexer).
-export([get_file_contents/1,show_file_contents/1,
	get_file_contents_by_line/1, test_suite/0]).

% File Indexer
% (Functional Programming in Erlang MOOC, Week 3 assignment)

%  This module builds on the provided functions  :
%	(get_file_contents, get_all_lines & show_file_contents)

% "The aim of this exercise is to index a text file, by line number.
%  In solving this problem you’ll need to think about the different stages of 
%  processing of the data: you begin with a list of lines, each of which will 
%  need to be broken into words, and those lines (and words) will need to be 
%  associated with the corresponding line numbers. 
%  So, thinking about useful intermediate stages – and helper functions – 
%  should help you to make progress in solving the problem. 

%  The output of the main function should be a list of entries consisting of a word
%  and a list of the ranges of lines on which it occurs. 
%  For example, the entry { "foo" , [{3,5},{7,7},{11,13}] } means that the word  
%  "foo"  occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the file. 

%  To take the problem further, you might like to think about these ways of 
%  refining the solution. 
%	• Removing all short words (e.g. words of length less than 3) or all common
%		words (you‘ll have to think about how to define these). 
%	• Sorting the output so that the words occur in lexicographic order. 
%	• Normalising the words so that capitalised ("Foo") and non-capitalised 
%		versions ("foo") of a word are identified. 
%	• Normalising so that common endings, plurals, etc. are identified. 
%	• (Harder) Thinking how you could make the data representation more 
%	efficient than the one you first chose. This might be efficient for lookup 
%	only, or for both creation and lookup. 
%	• Can you think of other ways that you might extend your solution?

% Exports the indexed words of the file as a list of entries indicating the 
%   range of lines they appear on. Example of one entry in this list :
%	{ "foo" , [{3,5},{7,7},{11,13}] }


get_file_contents_by_line(Name) ->
   LinesText = get_file_contents(Name),
   MapOfWordsToListOfReversedRangeTuples = map_line_words(LinesText),
   %io:format("Results : ~p ~n", [MapOfWordsToListOfReversedRangeTuples]),
   build_output(MapOfWordsToListOfReversedRangeTuples).
   %io:format("Results : ~p ~n", [Output]).

% Build the output from the map provided : specifically, each key in the map
% is build up as part of a tuple with its value; first the map value list needs to be reversed

build_output(M) ->
   build_output(M, []).
build_output(M, L) ->
   maps:fold(
   	fun(K, V, _) ->
   		Result = {K, reverse(V)},		% build the tuple
   		io:format("~p ~n", [Result])   		
	end, [], M).
	
	

% Takes a list of all lines in a file, and returns a mapping of the all words 
% to a list of tuples indicating the line ranges

%-spec map_line_words([string()]) -> #{string() => [integer()]}.
map_line_words(Xs) -> map_line_words(Xs, 1, #{}).   

%-spec map_line_words([string()], integer(), #{string() => [integer()]}) 
%						-> #{string() => [integer()]}.
map_line_words([], _C, Map) 	-> Map;
map_line_words([X|Xs], C, Map) 	->	
	CurrWords = sanatize_words(
				get_words_in_line(X)),	
	% io:format("Line : ~p : ~p ~p ~n", [C, X, CurrWords]),
	UpdatedMap = map_words(CurrWords, C, Map),
	
   map_line_words(Xs, C+1, UpdatedMap).
   

% Convenience func to sanatize the list of words provided

-spec sanatize_words([string()]) -> [string()].
sanatize_words(Xs) ->
   	lowercase_words(
   	   filter_small_words(
     	      strip_punc_in_words(Xs))).
	  
% filter small words from a list
filter_small_words([]) -> [];
filter_small_words(Xs) -> filter_small_words(Xs, []).
filter_small_words([], A) -> A;
filter_small_words([X|Xs], A) ->
   case (string:length(X) > 3) of
      true -> 
         filter_small_words(Xs, [X|A]);
      false -> 
         filter_small_words(Xs, A)
   end.


% Strip known punctuation from the supplied word, matching each char

-spec strip_punc(string()) -> string().
strip_punc(Word) -> 
	lists:filter( fun(C) -> 
				not lists:member(C, ".,\ ;:\t\n\'\"") 
			end, 
		Word ).


% Strip known punctuation from the supplied list.

-spec strip_punc_in_words([string()]) -> [string()].
strip_punc_in_words([]) -> [];
strip_punc_in_words(Words) -> 
   lists:map( fun(W) -> strip_punc(W) end,
   		Words).

% lowercase all words in the supplied list.
-spec lowercase_words([string()]) -> [string()].
lowercase_words([]) -> [];
lowercase_words(Words) -> 
   lists:map( fun(W) -> string:lowercase(W) end,
   		Words).

% Update the provided map M, mapping W to a list of values (integers);
% C is pre-pended to the values. If the key W exists, the value
% list is updated, otherwise the key is inserted with the new value

-spec map_word(string(), integer(), #{string() => [integer()]}) 
	-> #{string() => [integer()]}.
map_word(W, C, M) ->
   maps:update_with(W, fun(V) ->
   			[C|V]
   			end,
   		[C|[]], M).

% map a word (key) to a range of lines it appears on (list of tuples)
map_word_to_range_list(W, C, M) ->     
   maps:update_with(W, fun(V) ->
   			 create_or_update_ranges(V, C)
   			end,
   			create_or_update_ranges([], C),
   			 M).

% Update the provided map M for all words in the list, updating the values of range tuples for the word

%-spec map_words([string()], integer(), #{string() => [{integer(),integer()}]}
%	-> #{string() => [{integer(),integer()}]}.
map_words([], _C, M) -> M;
map_words([X|Xs], C, M) -> 
   UpdatedMap = map_word_to_range_list(X, C, M),
   map_words(Xs, C, UpdatedMap).
   	

% Return the words in a string as a list
	%uses a regex to split the line into a list of words
	%to ensure it does not return binaries, we can use split/3,
	%supplying Options to ask for a list return type
	% additionally, the trim options avoids getting an empty list in the result
	% without it, the following would match:
		%["trailing", ":", []] = get_words_in_line("trailing : "),

-spec get_words_in_line(string()) -> [string()].
get_words_in_line(Line) ->
   re:split(Line, " ", [{return, list}, trim]).


% Update or create a range in the provided list given the line number.
% We scan the list from the head, and check the ranges existing, if any.
% If none, we create one with the line number as start & end of the range.
% If found, we inspect the range end - if it's equal, we exit;
% if it is contiguous with the provided line number, we update it, 
% otherwise we insert a new tuple at head and return.
% This assumes the list is in reverse order (head == last inserted pair)

-spec create_or_update_ranges([{integer(),integer()}], integer()) ->
							[{integer(),integer()}].
create_or_update_ranges([], I) -> 
   %no ranges yet, create one.
   [{I,I}];				
create_or_update_ranges([{H,I}|Xs], I) -> 
   % the end range in the tuple at head matched our input, nothing to do, return the list
   [{H,I}|Xs];			
   % here we use a pattern match to indicate the end range is not the same as 
   % the provided integer AND a guard clause that satisfies the condition
   % that the provided int is contiguous; this saves us a case statement in the
   % function body, and simplifies our last function clause !
create_or_update_ranges([{H,I}|Xs], J) when (I+1 == J) -> 
   % update the tuple with the new end & return.  
   [{H,J}|Xs];
create_or_update_ranges(Xs, J) ->
   %all other matches have failed, simply concat a new tuple to head
   [{J,J}|Xs].   


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

-spec get_file_contents(string()) -> [string()].
get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).


% Auxiliary function for get_file_contents.
% Not exported.

-spec get_all_lines(string(),[string()]) -> [string()].
get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
               % Note : there is a bug here - depending on whether
               % the last char in Line is "\n", it should be 
               % length(Line) or length(Line)-1
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.


% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    
    
reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).
   

% ======== TESTS ========
test_map_word_to_range_list() ->
   M = map_word_to_range_list("Getting", 1, #{}),
   #{"Getting" := [{1,1}]} = M,
   MUpdated = map_word_to_range_list("Getting", 2, M),
   #{"Getting" := [{1,2}]} = MUpdated,
   MUpdated2 = map_word_to_range_list("This", 10, MUpdated),
   [{1,2}] = maps:get("Getting", MUpdated2),
   [{10,10}] = maps:get("This", MUpdated2),
   MUpdated3 = map_word_to_range_list("This", 12, MUpdated2),
   [{1,2}] = maps:get("Getting", MUpdated3),
   [{12,12},{10,10}] = maps:get("This", MUpdated3),
   pass.

  test_create_or_update_ranges() ->
   	%empty list, a new range is created
      [{1,1}] = create_or_update_ranges([],1),      
      	%the end range is the same as the int provided, no change
      [{2,2}] = create_or_update_ranges([{2,2}],2),
      	%the end range is less and contiguous to the int provided, end range updated
      [{2,3}] = create_or_update_ranges([{2,2}],3),
      	%the end range is not contiguous, a new tuple should be created
      [{4,4},{2,2}] = create_or_update_ranges([{2,2}],4),
      pass.

test_map_word() ->
   M = map_word("initial", 1, #{}),
   #{ "initial" := [1] } = M,
   MUpdate = map_word("initial", 5, M),
   #{ "initial" := [5,1] } = MUpdate,
   MUpdate2 = map_word("second", 1, MUpdate),
   #{ "initial" := [5,1], "second" := [1]} = MUpdate2,
   pass.
   
test_lowercase_words() ->
   ["this was a test"] = lowercase_words(["this Was A TesT"]),
   pass.

test_filter_small_words() ->
   ["test","this"] = filter_small_words(["this", "is", "a", "test"]),
   pass.
   
test_strip_punc() ->
   "PlsStriiMe" = strip_punc("Pls'Stri;i.Me,"),
   pass.
   
test_strip_punc_in_words() ->
   ["PlsStri", "iMe"] = strip_punc_in_words(["Pls'Stri", ";i.Me"]),
   pass.

test_get_words_in_line() ->
   ["sam", "i", "am"] = get_words_in_line("sam i am"),
   ["trailing", "punc."] = get_words_in_line("trailing punc."),
   ["trailing", ":"] = get_words_in_line("trailing : "),
   ["trailing", ":"] = get_words_in_line("trailing :"),
   pass.
   
      
% ======== TEST SUITE ========
 test_suite() ->
    pass = test_map_word_to_range_list(),
    pass = test_filter_small_words(),
    pass = test_create_or_update_ranges(),
    pass = test_get_words_in_line(),   
    pass = test_strip_punc(),
    pass = test_strip_punc_in_words(),
    pass = test_lowercase_words(),
    pass = test_map_word(),
    pass.
