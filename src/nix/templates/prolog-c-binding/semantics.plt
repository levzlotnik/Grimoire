% Tests for Prolog-C binding pattern
% Validates all C foreign predicates and ECS integration

% Load the main semantics file first
:- consult('semantics.pl').

:- begin_tests(prolog_c_binding).

% Test auxiliary module exports expected predicates
test(auxiliary_exports) :-
    current_predicate(auxiliary:add_numbers/3),
    current_predicate(auxiliary:multiply_floats/3),
    current_predicate(auxiliary:string_length/2),
    current_predicate(auxiliary:atom_concat_c/3),
    current_predicate(auxiliary:list_sum/2),
    current_predicate(auxiliary:list_reverse/2),
    current_predicate(auxiliary:make_compound/3),
    current_predicate(auxiliary:decompose_compound/4),
    current_predicate(auxiliary:is_even/1),
    current_predicate(auxiliary:factorial/2),
    current_predicate(auxiliary:generate_range/3),
    current_predicate(auxiliary:find_divisors/2),
    current_predicate(auxiliary:unify_structure/2).

% Test entity declarations
test(entities_exist) :-
    entity(calculator),
    entity(text_processor),
    entity(list_processor),
    entity(term_manipulator),
    entity(number_analyzer).

% Test arithmetic operations
test(add_numbers_basic) :-
    add_numbers(5, 3, Result),
    Result =:= 8.

test(add_numbers_negative) :-
    add_numbers(-5, 3, Result),
    Result =:= -2.

test(add_numbers_zero) :-
    add_numbers(0, 42, Result),
    Result =:= 42.

test(multiply_floats_basic) :-
    multiply_floats(2.5, 4.0, Result),
    Result =:= 10.0.

test(multiply_floats_negative) :-
    multiply_floats(-1.5, 2.0, Result),
    Result =:= -3.0.

test(multiply_floats_zero) :-
    multiply_floats(0.0, 42.5, Result),
    Result =:= 0.0.

% Test string/atom operations
test(string_length_basic) :-
    string_length(hello, Length),
    Length =:= 5.

test(string_length_empty) :-
    string_length('', Length),
    Length =:= 0.

test(atom_concat_basic) :-
    atom_concat_c(hello, world, Result),
    Result == helloworld.

test(atom_concat_empty) :-
    atom_concat_c('', hello, Result),
    Result == hello.

% Test list operations
test(list_sum_basic) :-
    list_sum([1, 2, 3, 4, 5], Sum),
    Sum =:= 15.

test(list_sum_empty) :-
    list_sum([], Sum),
    Sum =:= 0.

test(list_sum_negative) :-
    list_sum([-1, -2, 3], Sum),
    Sum =:= 0.

test(list_reverse_basic) :-
    list_reverse([1, 2, 3], Reversed),
    Reversed == [3, 2, 1].

test(list_reverse_empty) :-
    list_reverse([], Reversed),
    Reversed == [].

test(list_reverse_single) :-
    list_reverse([42], Reversed),
    Reversed == [42].

% Test compound term operations
test(make_compound_basic) :-
    make_compound(person, [john, 25], Term),
    Term =.. [person, john, 25].

test(make_compound_no_args) :-
    make_compound(atom_term, [], Term),
    Term == atom_term.

test(decompose_compound_basic) :-
    decompose_compound(person(john, 25), Name, Arity, Args),
    Name == person,
    Arity =:= 2,
    Args == [john, 25].

test(decompose_compound_atom) :-
    decompose_compound(simple_atom, Name, Arity, Args),
    Name == simple_atom,
    Arity =:= 0,
    Args == [].

% Test deterministic predicates
test(is_even_true) :-
    is_even(4).

test(is_even_false) :-
    \+ is_even(5).

test(is_even_zero) :-
    is_even(0).

test(is_even_negative) :-
    is_even(-2).

test(factorial_basic) :-
    factorial(5, Result),
    Result =:= 120.

test(factorial_zero) :-
    factorial(0, Result),
    Result =:= 1.

test(factorial_one) :-
    factorial(1, Result),
    Result =:= 1.

% Test non-deterministic predicates
test(generate_range_basic) :-
    findall(X, generate_range(1, 5, X), Values),
    Values == [1, 2, 3, 4, 5].

test(generate_range_single) :-
    findall(X, generate_range(3, 3, X), Values),
    Values == [3].

test(generate_range_empty) :-
    findall(X, generate_range(5, 3, X), Values),
    Values == [].

test(find_divisors_basic) :-
    findall(D, find_divisors(12, D), Divisors),
    Divisors == [1, 2, 3, 4, 6, 12].

test(find_divisors_prime) :-
    findall(D, find_divisors(7, D), Divisors),
    Divisors == [1, 7].

test(find_divisors_one) :-
    findall(D, find_divisors(1, D), Divisors),
    Divisors == [1].

% Test unification
test(unify_structure_success) :-
    unify_structure(person(X, 25), person(john, Y)),
    X == john,
    Y =:= 25.

test(unify_structure_fail) :-
    \+ unify_structure(person(john, 25), animal(dog, 3)).

% Test ECS component integration
test(calculator_components) :-
    component(calculator, operation, addition),
    component(calculator, operation, multiplication).

test(text_processor_capabilities) :-
    component(text_processor, capability, length_calculation),
    component(text_processor, capability, concatenation).

test(list_processor_capabilities) :-
    component(list_processor, capability, summation),
    component(list_processor, capability, reversal).

test(term_manipulator_capabilities) :-
    component(term_manipulator, capability, construction),
    component(term_manipulator, capability, decomposition).

test(number_analyzer_capabilities) :-
    component(number_analyzer, capability, evenness_test),
    component(number_analyzer, capability, factorial_calculation),
    component(number_analyzer, capability, range_generation),
    component(number_analyzer, capability, divisor_finding).

% Test conjure spells
test(conjure_arithmetic_add) :-
    cast(conjure(arithmetic_operation(add, 10, 5)), Result),
    Result =:= 15.

test(conjure_arithmetic_multiply) :-
    cast(conjure(arithmetic_operation(multiply, 2.5, 3.0)), Result),
    Result =:= 7.5.

test(conjure_text_length) :-
    cast(conjure(text_operation(length, testing)), Length),
    Length =:= 7.

test(conjure_text_concat) :-
    cast(conjure(text_operation(concat, hello, world)), Result),
    Result == helloworld.

test(conjure_list_sum) :-
    cast(conjure(list_operation(sum, [10, 20, 30])), Total),
    Total =:= 60.

test(conjure_list_reverse) :-
    cast(conjure(list_operation(reverse, [a, b, c])), Reversed),
    Reversed == [c, b, a].

test(conjure_term_make) :-
    cast(conjure(term_operation(make, test, [arg1, arg2])), Term),
    Term =.. [test, arg1, arg2].

test(conjure_term_decompose) :-
    cast(conjure(term_operation(decompose, example(x, y, z))), parts(Name, Arity, Args)),
    Name == example,
    Arity =:= 3,
    Args == [x, y, z].

test(conjure_number_is_even) :-
    cast(conjure(number_analysis(is_even, 8)), Result),
    Result == true.

test(conjure_number_is_odd) :-
    cast(conjure(number_analysis(is_even, 7)), Result),
    Result == false.

test(conjure_number_factorial) :-
    cast(conjure(number_analysis(factorial, 4)), F),
    F =:= 24.

test(conjure_number_range) :-
    cast(conjure(number_analysis(range, 2, 4)), Values),
    Values == [2, 3, 4].

test(conjure_number_divisors) :-
    cast(conjure(number_analysis(divisors, 6)), Divisors),
    Divisors == [1, 2, 3, 6].

% Test perception spells
test(perceive_operations) :-
    query(perceive(available_operations(calculator)), Operations),
    member(addition, Operations),
    member(multiplication, Operations).

test(perceive_unification) :-
    query(perceive(test_unification(X, test_value)), unified),
    X == test_value.

% Test docstrings exist
test(docstrings) :-
    docstring(prolog_c_domain, Doc1),
    atom_string(Doc1, _),
    docstring(calculator, Doc2),
    atom_string(Doc2, _),
    docstring(text_processor, Doc3),
    atom_string(Doc3, _),
    docstring(list_processor, Doc4),
    atom_string(Doc4, _),
    docstring(term_manipulator, Doc5),
    atom_string(Doc5, _),
    docstring(number_analyzer, Doc6),
    atom_string(Doc6, _).

:- end_tests(prolog_c_binding).