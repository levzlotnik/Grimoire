% Domain semantics with Prolog-C binding pattern
% This template demonstrates integration of C foreign predicates into ECS architecture

% Conditional self_entity declaration for Grimoire compatibility
:- catch(self_entity(prolog_c_domain), _, true).

% Allow discontiguous component/3 clauses
:- discontiguous component/3.

% Import C predicates from auxiliary module
:- use_module('auxiliary.pl', [
    add_numbers/3,
    multiply_floats/3,
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

% Example entities demonstrating C predicate usage
entity(calculator).
entity(text_processor).
entity(list_processor).
entity(term_manipulator).
entity(number_analyzer).

% Calculator entity using arithmetic C predicates
component(calculator, operation, addition).
component(calculator, operation, multiplication).

% Arithmetic operations through C binding
component(calculator, compute, Result) :-
    component(calculator, operands, [A, B]),
    component(calculator, operation, addition),
    add_numbers(A, B, Result).

component(calculator, compute, Result) :-
    component(calculator, operands, [A, B]),
    component(calculator, operation, multiplication),
    multiply_floats(A, B, Result).

% Text processor using string/atom C predicates
component(text_processor, capability, length_calculation).
component(text_processor, capability, concatenation).

component(text_processor, analyze, length(Text, Length)) :-
    string_length(Text, Length).

component(text_processor, transform, concat(A, B, Result)) :-
    atom_concat_c(A, B, Result).

% List processor using list C predicates
component(list_processor, capability, summation).
component(list_processor, capability, reversal).

component(list_processor, process, sum(List, Total)) :-
    list_sum(List, Total).

component(list_processor, process, reverse(Input, Output)) :-
    list_reverse(Input, Output).

% Term manipulator using compound term C predicates
component(term_manipulator, capability, construction).
component(term_manipulator, capability, decomposition).

component(term_manipulator, construct, compound(Name, Args, Term)) :-
    make_compound(Name, Args, Term).

component(term_manipulator, decompose, parts(Term, Name, Arity, Args)) :-
    decompose_compound(Term, Name, Arity, Args).

% Number analyzer using deterministic and non-deterministic C predicates
component(number_analyzer, capability, evenness_test).
component(number_analyzer, capability, factorial_calculation).
component(number_analyzer, capability, range_generation).
component(number_analyzer, capability, divisor_finding).

component(number_analyzer, test, even(N)) :-
    is_even(N).

component(number_analyzer, calculate, factorial(N, F)) :-
    factorial(N, F).

component(number_analyzer, generate, range(Start, End, Values)) :-
    findall(X, generate_range(Start, End, X), Values).

component(number_analyzer, find, divisors(N, Divisors)) :-
    findall(D, find_divisors(N, D), Divisors).

% Conjure spells for C predicate operations
component(conjure, ctor, arithmetic_operation).
component(conjure, ctor, text_operation).
component(conjure, ctor, list_operation).
component(conjure, ctor, term_operation).
component(conjure, ctor, number_analysis).

% Execute arithmetic operations
cast(conjure(arithmetic_operation(add, A, B)), Result) :-
    add_numbers(A, B, Result).

cast(conjure(arithmetic_operation(multiply, A, B)), Result) :-
    multiply_floats(A, B, Result).

% Execute text operations
cast(conjure(text_operation(length, Text)), Length) :-
    string_length(Text, Length).

cast(conjure(text_operation(concat, A, B)), Result) :-
    atom_concat_c(A, B, Result).

% Execute list operations
cast(conjure(list_operation(sum, List)), Total) :-
    list_sum(List, Total).

cast(conjure(list_operation(reverse, List)), Reversed) :-
    list_reverse(List, Reversed).

% Execute term operations
cast(conjure(term_operation(make, Name, Args)), Term) :-
    make_compound(Name, Args, Term).

cast(conjure(term_operation(decompose, Term)), parts(Name, Arity, Args)) :-
    decompose_compound(Term, Name, Arity, Args).

% Execute number analysis
cast(conjure(number_analysis(is_even, N)), Result) :-
    (is_even(N) -> Result = true ; Result = false).

cast(conjure(number_analysis(factorial, N)), F) :-
    factorial(N, F).

cast(conjure(number_analysis(range, Start, End)), Values) :-
    findall(X, generate_range(Start, End, X), Values).

cast(conjure(number_analysis(divisors, N)), Divisors) :-
    findall(D, find_divisors(N, D), Divisors).

% Perception spells for querying C predicate capabilities
component(perceive, ctor, available_operations).
component(perceive, ctor, test_unification).

% Query available operations
query(perceive(available_operations(Entity)), Operations) :-
    entity(Entity),
    findall(Op, component(Entity, capability, Op), Operations).

% Test unification through C
query(perceive(test_unification(Term1, Term2)), unified) :-
    unify_structure(Term1, Term2).

% Docstrings
docstring(prolog_c_domain,
   'Template domain demonstrating Prolog-C foreign function interface.\n\
   Shows integration of C predicates into ECS architecture with:\n\
   - Arithmetic operations (integers and floats)\n\
   - String/atom manipulation\n\
   - List processing\n\
   - Compound term construction/decomposition\n\
   - Deterministic and non-deterministic predicates\n\
   - Unification examples\n\
   All C interactions are imported through auxiliary.pl module.').

docstring(calculator,
   'Calculator entity using C foreign predicates for arithmetic operations.\nSupports addition of integers and multiplication of floats.').

docstring(text_processor,
   'Text processor entity using C foreign predicates for string/atom operations.\nSupports length calculation and concatenation.').

docstring(list_processor,
   'List processor entity using C foreign predicates for list operations.\nSupports summation and reversal of lists.').

docstring(term_manipulator,
   'Term manipulator entity using C foreign predicates for compound term operations.\nSupports construction and decomposition of compound terms.').

docstring(number_analyzer,
   'Number analyzer entity using C foreign predicates for number analysis.\nSupports evenness testing, factorial calculation, range generation, and divisor finding.\nDemonstrates both deterministic and non-deterministic C predicates.').
