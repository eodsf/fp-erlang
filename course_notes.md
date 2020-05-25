# Comprehensive notes from the Erlang MOOC

This file captures important notes from the course, week by week, capturing the essence of the content from Prof Simon Thompson, 
as well as insightful comments from one of the moderators from the EEF, Brujo Benavides (blog link in the readme).


## Week 1 - A tour of the language, intro to pattern matching

The first chunk of this week was focused on history, culture, setting up an environment.
Coninuing into writing some simple functions, exporting them from the module, and calling them from the shell.

(Supporting handout "Common_errors_in_Erlang.pdf" is useful for basic problems you might first encounter.)

Moving on to the different types in the language :
**Numbers** - *integers* (bignums w/ abritrarily large precision; bases can be specified as <base>#<number>) & *floats*.
**Atoms** - they stand alone - just a piece of data, can be tested for equality, and **used for pattern matching**.
**Booleans** - special atoms
**Tuples** - a number of pieces of data; a common idiom : first field generally *describes* the type (eg, see the complex types used in exercises such as circle, etc).
              Typically hetrogeneous : diff types in diff fields.
**Lists** - a collection of values. *Typically* homogenous (all of the same type).
(Although lists & tuples might appear to serve similar functionality (eg, they hold collections of values, which can 
  be of different types), they are used differently ... lists are built up one at a time)
**String** - lists of chars  
**Functions** - can be data themselves, eg, args to other functions -> powerful data maniuplation (eg, map reduce).
(see "9.Erlangdatatupleslistsandfunctions.mp4")

Moving onto **variables & pattern matching**. Variables are single assignment only ... if not *bound*, equality ("=") gives it 
  a value, otherwise it's a check on its value (pattern match). Variables are "much more like definions".
*Patterns can be used on the lhs of an assignment*, which results in destructuring, eg:
> {A,B}={2,3}                                 (===> A is assigned 2, B is assigned 3)
*Note: if any of the pattern match fails (eg, A is already bound), the whole expression fails.

**Pattern matching** (along with **guards** in function headers) is a very powerful tool in function clauses;
patterns are matched sequentially (so best to put more specifc matches first). *If possible, try to get as far with pattern matching
as possible, before resorting to guards, and then case statements in the function body.*
(good, succinct examples in "11.Patternmatching.mp4", and in "12.Summingup.mp4")

(Note : using **=:=** will ensure only the same terms match - it prevents surprises when attempting to match 1.0 (float) to 1 (int) 
  for ex)

## Week 2 - Recursion, lists

(
If you're sure how a particular input to a function should be handled *let it fail* (eg, ensure it matches nothing); handle *expected*
parameters correctly, let unexpected ones crash (putting the onus the caller) [specs can be added to the function to make it more 
explicit what types/ranges are expected; Brujo : "Then you can use tools like dialyzer to verify that nobody is accidentally using this 
function with a negative value".
)

2 different styles of recursion : *direct* (a more 'direct' description of the problem) & *tail* (optomized).
Tail recursion is the functional version of a loop; an accumulator is used & passed into each call. Erlang provides TCO to use the 
same stack frame, so this is more efficient.

(for a very succint tail recursion of fib (and perfect), see vid "5.Tailrecursionfeedback.mp4" - last 2 values in the seq are 'carried').
[
  it helped me to think of TC definitions like this as loops (although backwards (N to 0) in this case), with the 'base cases' built
  first as you might do in 'bottom up dynamic prog' solutions & passed as the accumulator
]
(and then, in "6.Patternmatchingrevisited.mp4", fib is re-visited, where the recursion is re-worked to make it a little more 
  direct & expressive (not a tail recursion though) - the pairs are returned as the result, and the fib value is extracted using a 
  pattern match ! Beautiful.
)

**Lists** : are ordered, and multiplicity matters. 
  Are either empty or not; if non-empty, it *always* matches the pattern [X|Xs], where X selects the head, and Xs
  the tail.
  Lists can be built up using the cons operator "|".
 Examples are given using lists in direct & tail recursion. Lots of exercises & video examples of different scenarios for reference. 
 In "Morefunctionsoverlists.mp4" some nice examples of using pattern matching & function composition to get all areas of any shape
  from a list of shapes -> mapping the 'area' function to all elements of the list, building up a list of results as we recurse.

Moving on, patterns for defining functions are visited. 
 In "Towardspatternsfordefiningfunctions.mp4", some nice constructs are introduced when pattern matching, such as, if the same 
  pattern match is used in the same function clause multiple times, binding it and using the bound var name in the other places.
  (see also "shapes.erl"). Introduction of case statments can make things even more compact.

3 general categories of patterns of recursion over lists:
- summing - combining elements in some way.
- transformations - 
- filtering / selecting

Revisiting the difference between lists & tuples:
 - lists are typically (de)constructed ele by ele
 - tuples are (de)constructed all at once.
 
Next, in a section called "Where Do I Begin", some general stategies are introduced for defining functional programs through examples.
**spec** is introduced:
```
  -spec take(integer(),[T]) -> [T].
```

(Brujo: "Tip: Remember that besides the type integer(), there is also pos_integer() and non_neg_integer().")

In defining a 'take' function, N and the list are recursed over - very nice; and then looks at how a std lib function can
be re-used (in this case lists:split)  
In defining a 'nub' function, very nice strategies used to 'think of what supporting functions we need', and define them as we go.
  A clear decomposition of the problem into simpler terms with supporting functions instead of big case statements.
In defining a palindrome function, the problem is first broken down into discrete steps:
  - solve the 'literal palindrome' first - solve for only same case, no punctuation; first a 'shunt' func is defined to reverse a list,
    which is then used to simply see if the reverse of the param is the same as the param
  - then apply funcs 'nocaps' (transform) & 'nopunc' (filter) using function composition to the func above for the final solution.
    (as noted, the 2 aforementioned functions can be combined, but readability suffers; filtering should be done first for efficiency if
      this approach is taken).

(see  where1.erl, where2.erl, where3.erl "Definingthetakefunction.mp4", "Definingthenubfunction.mp4", "Definingthepalindromefunction.mp4")

In a special week 2 feedback video, Simon recaps the important concepts introduced w/ some live programming, inc encouraging more use
of pattern matching for:
 - testing for equality; and repeated vars
 - distinguishing between cases
 - extracting elements / components from structures
 - makes it very clear about the components we're not interested in (by using underscores with them)
Example :
% List = [ {foo, 3}, {bar, 27}, {foo, 5} ]
% define a func to pull out all values for foo; note that instead of A|As we can use {A,V} since we know that the head will contain a 
% pair - an atom & a value ! Then we can easily distinguish between the 2 cases (a match or not on the head 1st pair ele w/ the 
%       input, or not !
```
lookup(A,[]) -> [];
lookup(A, [{A,V} | Xs])-> [V | lookup(A, Xs)];
lookup(A, [{_B,_V} | Xs])-> [lookup(A, Xs)].
```
(https://www.youtube.com/watch?v=Xa_ixfEF7qk&feature=youtu.be)

## Week 3
Started out with an assignment to write a file indexer. More challenging than the previous exercises, and time-consuming ;-)
Moving onto discsusion of testing, with the std framework EUnit, and property based testing (commercial & OS [QuickCheck Mini & Proper]). Some refs:
  https://proper-testing.github.io/
  http://www.quviq.com/downloads/
  https://gist.github.com/efcasado/3df8f3f1e33eaa488019
  https://propertesting.com/
  https://medium.com/erlang-battleground/property-based-testing-erlang-elixir-de72ad24966b
  
Revisiting types ... since Erlang is weakly typed, it's adviasble to lean on some tools such as Typer & Dialyzer to try & minimize the occurrence of runtime type errors.
(see "Types for functional programs FL version1.mp4")
A lot of this week was small assignments to consolidate what was learned in the first two weeks : lists & how they are the workhorses 
of and can be used in recursion.
Summing up video: https://www.youtube.com/watch?v=BUZAcnCMl30&feature=youtu.be




## Pro Tips (mainly from the moderators)

### On data types
**Brujo Benavides:**
"
Since "a string" is "a list of integers", then there is no way to guarantee that a function returns one and not the other.
On the other hand, you can use function specifications to document that fact, like…

-spec string_fun() -> string().
string_fun() -> "this function returns a string".

-spec list_fun() -> [non_neg_integer(), ...].
list_fun() -> [1,2,3].
"
"
"abc" _is_ [97,98,99]... just expressed in a different way.
So, anything you can do with [97,98,99], you can do with "abc" and [$a,$b,$c], and [$a|[98|"c"]]...

1> [H|T] = "abc".
"abc"
2> H.
97

On the other hand, if you want to _display_ the integer values of a list of chars, you can do something like this...
3> L = "abc".
"abc"
4> [an_atom|"abc"].
[an_atom,97,98,99]

i.e. Prepend the list with something that's not a character.
"
On Lists:
"
I think you might see it clearly in the following examples…

I'm gonna start with an empty list…
1> L0 = [].
[]

Now I'm gonna add an element to its head…

2> L1 = [x0 | L0].
[x0]

Note how L1 can also be represented as [x0 | []]

Now, let's add another element to its head…

3> L2 = [x1 | L1].
[x1,x0]

See? Now L2 can be written as [x1, x0], or [x1 | [x0]] or [x1 | [x0 | []]]

In a nutshell: [H|T] (cons), prepends H to the list T.
In other words: the expression [H|T] evaluates to a list that has H as its first element (its head) and T afterwards (its tail).
The issue with [3|4] is that… Erlang allows you to build improper lists like that one where the tail is not actually a list. They're not very useful, and generally considered a bad idea, but if you google "erlang improper list" you'll find a lot of conversations about them.

Now… let's check ++… starting with our last list above and attaching two elements to its tail…

4> L3 = L2 ++ [y1, y2].
[x1,x0,y1,y2]

See how the only thing that ++ does is it takes 2 lists and it returns a larger one?

The issue with [4] is that it's a list… but that doesn't matter…

6> [{a, tuple}] ++ [[a, list], an_atom].
[{a,tuple},[a,list],an_atom].
"
(
related:
Jeff Grunewald:
"
in general, you definitely don't want to use `++` unless you know the things on _both_ sides of the operation are lists to 
avoid the "improper list" like `[thing | other_thing]`.
the `|` is for prepending and will always produce a proper list when used in the context of `[ thing | other_thing ]`
"
)

"
Well, among other things… function names, module names, boolean values… they're all atoms.

Also, you can "tag" tuples with atoms, e.g.: {user, "elbrujohalcon", "Brujo Benavides"} so that you can later 
pattern-match on them with something like {user, Handle, Name} = SomeUser. This is a very common practice in Erlang. 
Pattern-Matching atoms is "faster" than pattern-matching strings or binaries, for instance.
"

**Antonio Cangiano:**
"It's worth noting that atoms are not garbage collected so, from a security standpoint, one must be careful not to convert 
user input into atoms. This is a typical concern in web programming where the user input can never be trusted."

Dairon Medina Caro:
"yeah that could potentially increase memory too. One good practice if you trust/know the input is to do call 
erlang:binary_to_existing_atom/2."

**Simon ThompsonLEAD EDUCATOR**:
"And just to confuse things, strings don't really exist in Erlang: they are "just" lists of ASCII codes …"

### On functions:

**Brujo Benavides**:
"
Left a comment on the gist, but to make it a bit more visible… Named (i.e. not anonymous) functions (the ones written in modules) 
can be passed around as first-class citizens, too… you just need to know the syntax:

fun a_function/1
^-- that's the local function a_function with arity 1.

fun a_module:a_function/2
^-- that's the function a_function with arity 2 from module a_module. NOTE: It must be exported for you to use it like that.
"

### Pattern matching:
(
In response to this q:
"
Just curious about something. So when Simon did this in the shell:

simple:howManyEqual(X, X)
why didn't he get the warning WARNING: Variable X not used.
Because in the definition we have something like this:
howManyEqual(X, X) -> 2;
howManyEqual(_X, _Y) -> 0.

Why not howManyEqual(_X, _X) -> 2?

The Erlang interpreter didn't mind that X didn't appear on the right-hand side. But the same interpreter complained because X and Y 
did not appear on the right hand side of the second definition and so had to replace with _X and _Y. Why is that? 
That appears to be something of an inconsistency in the logic of the interpreter. Can someone please explain that. Thank you!
"
)
**Brujo Benavides**:
"
@DouglasLewit let me see if I can shed some light here…
First, variables starting with underscores, are anonymous variables:
https://erlang.org/doc/reference_manual/expressions.html#variables

They're used to let the compiler know that it doesn't need to warn us if we don't _use_ them.

Variables in general are _used_ when we put them in a pattern or expression after they're bound.
The VM reads functions from top to bottom and from left to write… so… when evaluating things like simple:how_many_equal(1, 1)… it first checks the first argument of the first clause… it finds the pattern X, so it bounds X to 1. Then it finds X again in the second argument, since it's bound already… it checks if X = 1. Since it does… it returns 2.
Now, if you try with how_many_equal(1, 2), the first part is the same, but when it gets to the second argument, since 2 doesn't match with X (because X is bound to 1)… the clause doesn't match and the VM moves to the next one… matches _X to 1, _Y to 2… and returns 0.

Finally, remember that X in the shell is not the same as X in the function head. Those are 2 different variables. For instance…

1> X = 2.
2
2> simple:how_many_equal(1, 1). % X = 1 inside the fun
2
"
(in response to this q:
"
Señor @BrujoBenavides : I have two questions about the code in your blog post at 
https://medium.com/erlang-battleground/simple-tdd-in-erlang-5c0cca886acb :
1. In the test() function, you always put the expected result (true or false) on the left hand side of the evaluation and the function-under-test on the right hand side. 
Is there a reason for this or is it simply personal preference?
2. You reuse the name of the main function is_leap twice: first with one passed parameter, then with three. 
Does Erlang see (and therefore allow) is_leap/1 and is_leap/3 as two different atoms? Why not instead give the second function 
(only used internally to the module, not exported) a slightly different name?
Thank you in advance for your time addressing these questions.
")
**Brujo Benavides**:
"
1. Yes, because what's on the left side can be a pattern (i.e. it might have unbound variables), therefore you can write stuff like…

{rectangle, _, _, _} = ex:enclosing({circle, {0,0}, 1},

…if the only thing you want to assert is that the function returns a rectangle and you don't care about its properties.

2. Kinda. Erlang sees is_leap/1 and is_leap/3 as two different functions (not atoms). It's idiomatic in Erlang to use the same name 
with different arities for auxiliary functions. I'm not sure why tho. I guess… because we can ;)
"

### Lists:
**Brujo Benavides**:
"
quick tips:
1. instead of [0] ++ Acc... You can use [0|Acc] and build the list directly using cons.
2. Instead of using lists:nth/2 twice, you can pattern-match on the clause header...
f(N, [A, B | _] = Acc) -> [A+B | Acc].
"

#### List matching tips:
**Brujo Benavides**:
"
With two elements or more… [X,Y|_]
And… with one element or more… [_|_] <- I call this one, the robot butt :P
"
"
One thing that @SimonThompson glossed over in this video and that can be the source of confusion is that, as Simon clearly said, *EVERY* list that's not empty matches [X|Xs]. That includes lists with just one element. Check it out…

1> [X] = [elem_1].
[elem_1]
2> [X|Xs] = [elem_1].
[elem_1]
3> X.
elem_1
4> Xs.
[]

So… [elem_1] is actually exactly the same as [elem_1|[]]
"
"
Another two examples for the wat-seekers out there…

1> case [1, 2, 3, 4] of
1> [1, 2] ++ Tail -> Tail;
1> _ -> wat
1> end.
[3, 4]
2> case "this is a list" of
2> "this is " ++ WhatIsThis -> WhatIsThis;
2> _ -> {nothing, to, see, here}
2> end.
"a list"
"

### General:
**Brujo Benavides**:
"
A few notes regarding these exercises…

1. Stylistic/idiomatic approach: It's far more common in the Erlang community to use snake_case instead of camelCase for function names, module names and atoms in general.

2. You can use pattern-matching for tests, too… instead of equality. i.e. you can write `0 = how_many_equal(34, 25, 36)` instead of using `==` and then the test function will crash if you have an error instead of returning false.

3. As a matter of fact, if you're a TDD fan like myself you can totally write tests first like this:

-module(brujo).
-export [test/0].

test() ->
0 = brujo:how_many_equal(x, y, z),
2 = brujo:how_many_equal(x, x, w),
3 = brujo:how_many_equal(a, a, a),
all_good.

That module compiles and you can run it on your console and it will start by telling you which function you need to define…

1> c(brujo).
{ok,brujo}
2> brujo:test().
** exception error: undefined function brujo:how_many_equal/3
in function brujo:test/0 (brujo.erl, line 5)

Nice, isn't it?
"
