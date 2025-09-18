% Auxiliary module for Prolog-C binding
% Imports all C foreign predicates and provides clean interface

:- module(auxiliary, [
    % Arithmetic operations
    add_numbers/3,
    multiply_floats/3,
    
    % String/atom manipulation
    c_string_length/2,
    c_atom_concat/3,
    
    % List operations
    c_list_sum/2,
    c_list_reverse/2,
    
    % Compound term operations
    c_make_compound/3,
    c_decompose_compound/4,
    
    % Deterministic predicates
    c_is_even/1,
    c_factorial/2,
    
    % Non-deterministic predicates
    c_generate_range/3,
    c_find_divisors/2,
    
    % Unification examples
    c_unify_structure/2,
    
    % Convenience wrappers
    string_length/2,
    atom_concat_c/3,
    list_sum/2,
    list_reverse/2,
    make_compound/3,
    decompose_compound/4,
    is_even/1,
    factorial/2,
    generate_range/3,
    find_divisors/2,
    unify_structure/2
]).

% Load the C foreign library
% Try different paths depending on environment
:- ( exists_file('./build/prolog_c_binding.so') ->
       use_foreign_library('./build/prolog_c_binding.so')
   ; exists_file('./build/prolog_c_binding') ->
       use_foreign_library('./build/prolog_c_binding')
   ; use_foreign_library(foreign(prolog_c_binding))
   ).

% Wrapper predicates to provide clean interface
% (hides the c_ prefix from users)

% String/atom manipulation
string_length(String, Length) :- c_string_length(String, Length).
atom_concat_c(Atom1, Atom2, Result) :- c_atom_concat(Atom1, Atom2, Result).

% List operations
list_sum(List, Sum) :- c_list_sum(List, Sum).
list_reverse(Input, Output) :- c_list_reverse(Input, Output).

% Compound term operations
make_compound(FunctorName, Args, Result) :- c_make_compound(FunctorName, Args, Result).
decompose_compound(Compound, FunctorName, Arity, Args) :- c_decompose_compound(Compound, FunctorName, Arity, Args).

% Deterministic predicates
is_even(Number) :- c_is_even(Number).
factorial(N, Result) :- c_factorial(N, Result).

% Non-deterministic predicates
generate_range(Start, End, Current) :- c_generate_range(Start, End, Current).
find_divisors(Number, Divisor) :- c_find_divisors(Number, Divisor).

% Unification examples
unify_structure(Input, Pattern) :- c_unify_structure(Input, Pattern).

% Documentation for exported predicates

%! add_numbers(+A:integer, +B:integer, -Result:integer) is det.
%
%  Add two integers and unify the result.
%  
%  @param A First integer
%  @param B Second integer  
%  @param Result Sum of A and B

%! multiply_floats(+A:float, +B:float, -Result:float) is det.
%
%  Multiply two floating-point numbers.
%
%  @param A First float
%  @param B Second float
%  @param Result Product of A and B

%! string_length(+String:atom, -Length:integer) is det.
%
%  Get the length of an atom or string.
%
%  @param String Input atom or string
%  @param Length Number of characters

%! atom_concat(+Atom1:atom, +Atom2:atom, -Result:atom) is det.
%
%  Concatenate two atoms.
%
%  @param Atom1 First atom
%  @param Atom2 Second atom
%  @param Result Concatenated atom

%! list_sum(+List:list(integer), -Sum:integer) is det.
%
%  Sum all integers in a list.
%
%  @param List List of integers
%  @param Sum Total sum

%! list_reverse(+Input:list, -Output:list) is det.
%
%  Reverse a list.
%
%  @param Input Original list
%  @param Output Reversed list

%! make_compound(+FunctorName:atom, +Args:list, -Result:compound) is det.
%
%  Create a compound term from functor name and arguments.
%
%  @param FunctorName Name of the functor
%  @param Args List of arguments
%  @param Result Constructed compound term

%! decompose_compound(+Compound:compound, -FunctorName:atom, -Arity:integer, -Args:list) is det.
%
%  Decompose a compound term into its parts.
%
%  @param Compound Input compound term
%  @param FunctorName Name of the functor
%  @param Arity Number of arguments
%  @param Args List of arguments

%! is_even(+Number:integer) is semidet.
%
%  Test if a number is even.
%
%  @param Number Integer to test

%! factorial(+N:integer, -Result:integer) is det.
%
%  Calculate factorial of N.
%
%  @param N Non-negative integer
%  @param Result N factorial

%! generate_range(+Start:integer, +End:integer, -Current:integer) is multi.
%
%  Generate integers in range [Start, End] on backtracking.
%
%  @param Start Starting value (inclusive)
%  @param End Ending value (inclusive)
%  @param Current Generated value

%! find_divisors(+Number:integer, -Divisor:integer) is multi.
%
%  Find all positive divisors of a number on backtracking.
%
%  @param Number Positive integer
%  @param Divisor Divisor of Number

%! unify_structure(+Input:term, +Pattern:term) is semidet.
%
%  Unify two terms (demonstrates unification in C).
%
%  @param Input First term
%  @param Pattern Second term
