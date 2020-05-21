%% coding: latin-1
-module(file_indexer).
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

% Bugs to fix : i) original code - if the last line doesn't have a \n
%			the last char is stripped from the content.



% EOD HERE - INTIAL THOGHTS - should we use maps ?
% we can process the list of lines using list recursion, pasing an acc 
% so we know the line #, and for each line, access the words in the list.
% the list of words can be 'iterated' by selecting X|Xs
%	we could use a map to store/lookup words as we encounter them
%		pushing the line # into the value of the map ele
%			- the value will be a list of size 2, and we can
%			just update the tail with the new line #


% Exports the indexed words of the file as a list of entries indicating the 
%   range of lines they appear on. Example of one entry in this list :
%	{ "foo" , [{3,5},{7,7},{11,13}] }


get_file_contents_by_line(Name) ->
% this is a list, each ele being a line
   AllLines = get_file_contents(Name),
   process_line(AllLines).

process_line(Xs) -> process_line(Xs, 1, #{}).   

process_line([], _C, _Map) 	-> [];
process_line([X|Xs], C, Map) 	->	
	CurrWords = sanatize_words(get_words_in_line(X)),	
	% io:format("Line : ~p : ~p ~p ~n", [C, X, CurrWords]),
	map_words(CurrWords, C, Map),
	
   process_line(Xs, C+1, Map).
   

% Convenience func to sanatize the list of words provided

-spec sanatize_words([string()]) -> [string()].
sanatize_words(Xs) ->
   strip_punc_in_words(Xs).


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
   
   
% Update the provided map M, mapping all Words to a list of values (integers);
	% C is pre-pended to the values.

-spec map_words([string()], integer(), #{string() => [integer()]}) 
	-> #{string() => [integer()]}.
map_words([], _C, M) -> M;
map_words(Words, C, M) -> 
   %note : flatmap is not working with my tests, so using map & flatten  
   List = lists:map( fun(W) -> map_word(W, C, M) end,
   		Words),
	lists:flatten(List).
	

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

% ======== TESTS ========

test_map_word() ->
   M = map_word("initial", 1, #{}),
   #{ "initial" := [1] } = M,
   MUpdate = map_word("initial", 5, M),
   #{ "initial" := [5,1] } = MUpdate,
   MUpdate2 = map_word("second", 1, MUpdate),
   #{ "initial" := [5,1], "second" := [1]} = MUpdate2,
   pass.
   
   
test_map_words() ->
   #{} = map_words([], 1, #{}),
   [#{"one" := [1]}] = map_words(["one"], 1, #{}),
   M = map_words(["initial", "second"], 1, #{}),
   #{"initial" := [1], "second" := [1]} = M,
   MUpdate = map_words(["initial"], 5, M),
   #{"initial" := [5,1], "second" := [1]} = MUpdate,
   pass.
   
   
test_strip_punc() ->
   "PlsStriiMe" = strip_punc("Pls'Stri;i.Me,"),
   pass.
   
test_strip_punc_in_words() ->
   ["PlsStri", "iMe"] = strip_punc_in_words(["Pls'Stri", ";i.Me"]),
   pass.

%TODO - we will need to handle some of these cases
test_get_words_in_line() ->
   ["sam", "i", "am"] = get_words_in_line("sam i am"),
   ["trailing", "punc."] = get_words_in_line("trailing punc."),
   ["trailing", ":"] = get_words_in_line("trailing : "),
   ["trailing", ":"] = get_words_in_line("trailing :"),
   pass.
   
% ======== TEST SUITE ========
 test_suite() ->
    pass = test_get_words_in_line(),   
    pass = test_strip_punc(),
    pass = test_strip_punc_in_words(),
    pass = test_map_word(),
    pass = test_map_words(),
    pass.