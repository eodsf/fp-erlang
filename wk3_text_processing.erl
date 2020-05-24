-module(wk3_text_processing).
-export([process_file/2, test_suite/0]).

% Basic problem definition:
%"Define a function that takes an input file in Erlang as a string (list) of 
% characters. and a line length len (a positive integer) and which returns a list 
% of lines, each of which is filled to include the maximum number of words up to 
% the overall length len (including punctuation characters).

% You should think about how best to represent lines in solving this problem: 
% is a string the best representation or is there a better alternative?"
%
% "Taking it further: 
%  i) justification
%  To align the right-hand margin, the text is justified by adding extra 
%  inter-word spaces on all lines but the last.
%     Define a function that takes a line, represented as above, and justifies it 
%	so that when printed it has exactly the length len.
%  ii) adding commands
%  If we have different kinds of layout, then we need to be able to describe how a 
%	particular paragraph of text should be laid out. One way to do this is to add formatting 
%	commands of the form .XX on a line on their own. Commands could include:
%	* .VB for verbatim (so no formatting, just copy lines from input to output,
%	* .CV lines are copied from input to output but also each line is centred (if possible),
%	* .LP for lines filled and aligned to the left (as shown above),
%	* .RP for lines filled and aligned to the right, and
%	* .JU for lines filled and justified.
%    Define a function which takes a string containing text and formatting commands,
%	and which outputs a string representing the formatted input.

process_file(Filename, MaxLineLen) when MaxLineLen >0  ->
   Content = get_file_contents(Filename),
	%io:format("Read : ~p ~n", [Content]).
   process_line(Content, MaxLineLen).
   
process_line(S, SplitAtLen) -> 
   reverse(	
	process_line(strip_extra_spaces(S), SplitAtLen, [])).
process_line(S, SplitAtLen, A) ->
   Len = string:length(S),
   case (Len > SplitAtLen) of
      true ->  Removed = string:slice(S, 0, SplitAtLen),
      		process_line(string:slice(S, SplitAtLen),
      			SplitAtLen,
      			[Removed | A]);
      false ->   [S | A]
   end.


% Get the contents of a text file into a list of lines, 
% each list represents lines in the input that were separated by any \r\n

-spec get_file_contents(string()) -> [string()].
get_file_contents(Name) ->
    {ok,File} = file:read_file(Name),
    unicode:characters_to_list(File).
    

-spec strip_extra_spaces(string()) -> string().
strip_extra_spaces(S) ->
   re:replace(S, "(\\s+\\s+)", " ", [global,{return,list}]).


reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).
    
    
test_process_line() ->
   ["this ","is a ","long ","strin","g"] = process_line("this    is     a long string", 5),
   %["this is a ","line with ","line break","s"] = process_line("this is a\r\nline with line breaks",10),
   pass.
    
test_suite() ->
   test_process_line(),
   pass.
