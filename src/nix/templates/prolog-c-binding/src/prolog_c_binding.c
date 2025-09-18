/* SWI-Prolog C Foreign Function Interface Example
 * Demonstrates comprehensive Prolog-C integration patterns
 */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Forward declarations */
static foreign_t pl_add_numbers(term_t a, term_t b, term_t result);
static foreign_t pl_multiply_floats(term_t a, term_t b, term_t result);
static foreign_t pl_string_length(term_t string, term_t length);
static foreign_t pl_atom_concat(term_t atom1, term_t atom2, term_t result);
static foreign_t pl_list_sum(term_t list, term_t sum);
static foreign_t pl_list_reverse(term_t input, term_t output);
static foreign_t pl_make_compound(term_t functor_name, term_t args, term_t result);
static foreign_t pl_decompose_compound(term_t compound, term_t functor_name, term_t arity, term_t args);
static foreign_t pl_is_even(term_t number);
static foreign_t pl_factorial(term_t n, term_t result);
static foreign_t pl_generate_range(term_t start, term_t end, term_t current, control_t handle);
static foreign_t pl_find_divisors(term_t number, term_t divisor, control_t handle);
static foreign_t pl_unify_structure(term_t input, term_t pattern);

/* Arithmetic Operations */

/* Add two integers: add_numbers(+A, +B, -Result) */
static foreign_t pl_add_numbers(term_t a, term_t b, term_t result) {
    long int_a, int_b, sum;
    
    if (!PL_get_long(a, &int_a) || !PL_get_long(b, &int_b)) {
        return PL_warning("add_numbers/3: arguments must be integers");
    }
    
    sum = int_a + int_b;
    return PL_unify_integer(result, sum);
}

/* Multiply two floats: multiply_floats(+A, +B, -Result) */
static foreign_t pl_multiply_floats(term_t a, term_t b, term_t result) {
    double float_a, float_b, product;
    
    if (!PL_get_float(a, &float_a) || !PL_get_float(b, &float_b)) {
        return PL_warning("multiply_floats/3: arguments must be floats");
    }
    
    product = float_a * float_b;
    return PL_unify_float(result, product);
}

/* String/Atom Manipulation */

/* Get string length: string_length(+String, -Length) */
static foreign_t pl_string_length(term_t string, term_t length) {
    char *str;
    size_t len;
    
    if (!PL_get_chars(string, &str, CVT_ATOM | CVT_STRING)) {
        return PL_warning("string_length/2: first argument must be atom or string");
    }
    
    len = strlen(str);
    return PL_unify_integer(length, (long)len);
}

/* Concatenate atoms: atom_concat(+Atom1, +Atom2, -Result) */
static foreign_t pl_atom_concat(term_t atom1, term_t atom2, term_t result) {
    char *str1, *str2;
    char *concatenated;
    atom_t result_atom;
    int success;
    
    if (!PL_get_chars(atom1, &str1, CVT_ATOM) || 
        !PL_get_chars(atom2, &str2, CVT_ATOM)) {
        return PL_warning("atom_concat/3: arguments must be atoms");
    }
    
    concatenated = malloc(strlen(str1) + strlen(str2) + 1);
    if (!concatenated) {
        return PL_warning("atom_concat/3: memory allocation failed");
    }
    
    strcpy(concatenated, str1);
    strcat(concatenated, str2);
    
    result_atom = PL_new_atom(concatenated);
    success = PL_unify_atom(result, result_atom);
    
    free(concatenated);
    PL_unregister_atom(result_atom);
    
    return success;
}

/* List Operations */

/* Sum all numbers in a list: list_sum(+List, -Sum) */
static foreign_t pl_list_sum(term_t list, term_t sum) {
    term_t head = PL_new_term_ref();
    term_t tail = PL_new_term_ref();
    term_t current_list = PL_copy_term_ref(list);
    long total = 0;
    long value;
    
    while (PL_get_list(current_list, head, tail)) {
        if (!PL_get_long(head, &value)) {
            return PL_warning("list_sum/2: all list elements must be integers");
        }
        total += value;
        current_list = tail;
    }
    
    if (!PL_get_nil(current_list)) {
        return PL_warning("list_sum/2: argument must be a proper list");
    }
    
    return PL_unify_integer(sum, total);
}

/* Reverse a list: list_reverse(+Input, -Output) */
static foreign_t pl_list_reverse(term_t input, term_t output) {
    term_t head = PL_new_term_ref();
    term_t tail = PL_new_term_ref();
    term_t current_list = PL_copy_term_ref(input);
    term_t result = PL_new_term_ref();
    
    /* Start with empty list */
    if (!PL_put_nil(result)) {
        return FALSE;
    }
    
    /* Process each element, prepending to result */
    while (PL_get_list(current_list, head, tail)) {
        term_t new_list = PL_new_term_ref();
        if (!PL_cons_list(new_list, head, result)) {
            return FALSE;
        }
        result = new_list;
        current_list = tail;
    }
    
    if (!PL_get_nil(current_list)) {
        return PL_warning("list_reverse/2: argument must be a proper list");
    }
    
    return PL_unify(output, result);
}

/* Compound Term Construction and Decomposition */

/* Create compound term: make_compound(+FunctorName, +Args, -Result) */
static foreign_t pl_make_compound(term_t functor_name, term_t args, term_t result) {
    char *name;
    atom_t name_atom;
    functor_t functor;
    int arity;
    term_t arg_list = PL_copy_term_ref(args);
    term_t head = PL_new_term_ref();
    term_t tail = PL_new_term_ref();
    term_t compound = PL_new_term_ref();
    int i = 0;
    
    if (!PL_get_atom_chars(functor_name, &name)) {
        return PL_warning("make_compound/3: functor name must be an atom");
    }
    
    /* Count arguments */
    arity = 0;
    term_t temp_list = PL_copy_term_ref(args);
    while (PL_get_list(temp_list, head, tail)) {
        arity++;
        temp_list = tail;
    }
    
    if (!PL_get_nil(temp_list)) {
        return PL_warning("make_compound/3: arguments must be a proper list");
    }
    
    name_atom = PL_new_atom(name);
    functor = PL_new_functor(name_atom, arity);
    
    if (!PL_put_functor(compound, functor)) {
        PL_unregister_atom(name_atom);
        return FALSE;
    }
    
    /* Fill in arguments */
    i = 1;
    while (PL_get_list(arg_list, head, tail)) {
        term_t arg = PL_new_term_ref();
        if (!PL_get_arg(i, compound, arg) || !PL_unify(arg, head)) {
            PL_unregister_atom(name_atom);
            return FALSE;
        }
        i++;
        arg_list = tail;
    }
    
    PL_unregister_atom(name_atom);
    return PL_unify(result, compound);
}

/* Decompose compound term: decompose_compound(+Compound, -FunctorName, -Arity, -Args) */
static foreign_t pl_decompose_compound(term_t compound, term_t functor_name, term_t arity, term_t args) {
    functor_t functor;
    atom_t name_atom;
    int arity_int;
    term_t arg_list = PL_new_term_ref();
    
    if (!PL_get_functor(compound, &functor)) {
        return PL_warning("decompose_compound/4: first argument must be a compound term");
    }
    
    name_atom = PL_functor_name(functor);
    arity_int = PL_functor_arity(functor);
    
    if (!PL_unify_atom(functor_name, name_atom) ||
        !PL_unify_integer(arity, arity_int)) {
        return FALSE;
    }
    
    /* Build argument list */
    if (!PL_put_nil(arg_list)) {
        return FALSE;
    }
    
    for (int i = arity_int; i >= 1; i--) {
        term_t arg = PL_new_term_ref();
        term_t new_list = PL_new_term_ref();
        
        if (!PL_get_arg(i, compound, arg) ||
            !PL_cons_list(new_list, arg, arg_list)) {
            return FALSE;
        }
        arg_list = new_list;
    }
    
    return PL_unify(args, arg_list);
}

/* Deterministic Predicates */

/* Check if number is even: is_even(+Number) */
static foreign_t pl_is_even(term_t number) {
    long num;
    
    if (!PL_get_long(number, &num)) {
        return PL_warning("is_even/1: argument must be an integer");
    }
    
    return (num % 2 == 0) ? TRUE : FALSE;
}

/* Calculate factorial: factorial(+N, -Result) */
static foreign_t pl_factorial(term_t n, term_t result) {
    long num, fact = 1;
    
    if (!PL_get_long(n, &num)) {
        return PL_warning("factorial/2: first argument must be an integer");
    }
    
    if (num < 0) {
        return PL_warning("factorial/2: argument must be non-negative");
    }
    
    for (long i = 1; i <= num; i++) {
        fact *= i;
    }
    
    return PL_unify_integer(result, fact);
}

/* Non-deterministic Predicates */

/* Generate range of numbers: generate_range(+Start, +End, -Current) */
static foreign_t pl_generate_range(term_t start, term_t end, term_t current, control_t handle) {
    long start_val, end_val, current_val;
    
    switch (PL_foreign_control(handle)) {
        case PL_FIRST_CALL:
            if (!PL_get_long(start, &start_val) || !PL_get_long(end, &end_val)) {
                return PL_warning("generate_range/3: start and end must be integers");
            }
            
            if (start_val > end_val) {
                return FALSE;  /* No solutions */
            }
            
            if (!PL_unify_integer(current, start_val)) {
                return FALSE;
            }
            
            if (start_val == end_val) {
                return TRUE;  /* Last solution */
            } else {
                PL_retry_address((void *)(start_val + 1));
            }
            
        case PL_REDO:
            current_val = (long)PL_foreign_context_address(handle);
            if (!PL_get_long(end, &end_val)) {
                return FALSE;
            }
            
            if (!PL_unify_integer(current, current_val)) {
                return FALSE;
            }
            
            if (current_val == end_val) {
                return TRUE;  /* Last solution */
            } else {
                PL_retry_address((void *)(current_val + 1));
            }
            
        case PL_PRUNED:
            /* Cleanup if needed */
            return TRUE;
            
        default:
            return FALSE;
    }
}

/* Find divisors of a number: find_divisors(+Number, -Divisor) */
static foreign_t pl_find_divisors(term_t number, term_t divisor, control_t handle) {
    long num, div;
    
    switch (PL_foreign_control(handle)) {
        case PL_FIRST_CALL:
            if (!PL_get_long(number, &num)) {
                return PL_warning("find_divisors/2: first argument must be an integer");
            }
            
            if (num <= 0) {
                return FALSE;  /* No solutions for non-positive numbers */
            }
            
            /* Start with divisor 1 */
            if (!PL_unify_integer(divisor, 1)) {
                return FALSE;
            }
            
            if (num == 1) {
                return TRUE;  /* Only solution */
            } else {
                PL_retry_address((void *)2);
            }
            
        case PL_REDO:
            div = (long)PL_foreign_context_address(handle);
            if (!PL_get_long(number, &num)) {
                return FALSE;
            }
            
            /* Find next divisor */
            while (div <= num && (num % div != 0)) {
                div++;
            }
            
            if (div > num) {
                return FALSE;  /* No more solutions */
            }
            
            if (!PL_unify_integer(divisor, div)) {
                return FALSE;
            }
            
            if (div == num) {
                return TRUE;  /* Last solution */
            } else {
                PL_retry_address((void *)(div + 1));
            }
            
        case PL_PRUNED:
            /* Cleanup if needed */
            return TRUE;
            
        default:
            return FALSE;
    }
}

/* Unification Examples */

/* Unify with structure: unify_structure(+Input, +Pattern) */
static foreign_t pl_unify_structure(term_t input, term_t pattern) {
    return PL_unify(input, pattern);
}

/* Registration function */
install_t install_prolog_c_binding() {
    /* Arithmetic operations */
    PL_register_foreign("add_numbers", 3, pl_add_numbers, 0);
    PL_register_foreign("multiply_floats", 3, pl_multiply_floats, 0);
    
    /* String/atom manipulation */
    PL_register_foreign("c_string_length", 2, pl_string_length, 0);
    PL_register_foreign("c_atom_concat", 3, pl_atom_concat, 0);
    
    /* List operations */
    PL_register_foreign("c_list_sum", 2, pl_list_sum, 0);
    PL_register_foreign("c_list_reverse", 2, pl_list_reverse, 0);
    
    /* Compound term operations */
    PL_register_foreign("c_make_compound", 3, pl_make_compound, 0);
    PL_register_foreign("c_decompose_compound", 4, pl_decompose_compound, 0);
    
    /* Deterministic predicates */
    PL_register_foreign("c_is_even", 1, pl_is_even, 0);
    PL_register_foreign("c_factorial", 2, pl_factorial, 0);
    
    /* Non-deterministic predicates */
    PL_register_foreign("c_generate_range", 3, pl_generate_range, PL_FA_NONDETERMINISTIC);
    PL_register_foreign("c_find_divisors", 2, pl_find_divisors, PL_FA_NONDETERMINISTIC);
    
    /* Unification examples */
    PL_register_foreign("c_unify_structure", 2, pl_unify_structure, 0);
}