-module(listtest).
-export([f/1,g/1,h/2,i/2]).

f([X | []]) ->
 X;
f([X, Y | []]) ->
 X + Y;
f([X, Y | Z]) ->
 X + Y + f(Z).

g([_X]) ->
   foo;
g(_) ->
   baz.

h([],A) ->
    A;
h(L,A) ->
    [A | L].

i(A,_B) when is_atom(A) ->
    atom_to_list(A);
i(_A,B) ->
    B.
