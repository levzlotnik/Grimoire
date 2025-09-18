% PyTorch C++ Extension Template Tests
% PLUnit tests for the template semantics

:- begin_tests(pytorch_extension).
:- use_module(library(plunit)).

% Load the main semantics
:- ensure_loaded(semantics).

% Test entity declarations
test(entities_exist) :-
    entity(parametric_swish),
    entity(fused_attention),
    entity(pytorch_custom_ops).

% Test parametric swish components
test(parametric_swish_components) :-
    component(parametric_swish, operation_type, activation_function),
    component(parametric_swish, mathematical_form, 'f(x) = x * sigmoid(beta * x)'),
    component(parametric_swish, autograd_support, true),
    component(parametric_swish, cpu_implementation, openmp_parallel).

% Test fused attention components
test(fused_attention_components) :-
    component(fused_attention, operation_type, attention_mechanism),
    component(fused_attention, mathematical_form, 'Y = softmax(Q @ K^T / scale) @ V'),
    component(fused_attention, autograd_support, true),
    component(fused_attention, cpu_implementation, blas_optimized).

% Test extension module components
test(extension_module_components) :-
    component(pytorch_custom_ops, module_type, cpp_extension),
    component(pytorch_custom_ops, build_system, torch_utils_cpp_extension),
    component(pytorch_custom_ops, supported_operations, [parametric_swish, fused_attention]).

% Test operation input/output specifications
test(parametric_swish_specs) :-
    component(parametric_swish, input_spec, tensor(shape([any]), dtype(float32))),
    component(parametric_swish, output_spec, tensor(shape([same_as_input]), dtype(same_as_input))),
    component(parametric_swish, parameter_spec, [beta(float, range(0.1, 10.0))]).

test(fused_attention_specs) :-
    component(fused_attention, input_spec, InputSpec),
    InputSpec = [
        query(tensor(shape([batch, seq_len, d_model]), dtype(float32))),
        key(tensor(shape([batch, seq_len, d_model]), dtype(same_as_query))),
        value(tensor(shape([batch, seq_len, d_value]), dtype(same_as_query)))
    ].

% Test performance characteristics
test(performance_profiles) :-
    component(parametric_swish, performance_profile, Profile1),
    memberchk(medium_tensors(speedup_range(1.2, 2.0)), Profile1),
    component(fused_attention, performance_profile, Profile2),
    memberchk(seq_len_128(speedup_range(1.3, 2.0)), Profile2).

% Test complexity specifications
test(complexity_specs) :-
    component(parametric_swish, complexity, o_n),
    component(fused_attention, complexity, o_n2_d).

% Test build requirements
test(build_requirements) :-
    component(pytorch_extension, build_requirements, Requirements),
    memberchk(cmake, Requirements),
    memberchk(openmp, Requirements),
    memberchk(pytorch_dev, Requirements).

% Test testing configuration
test(test_configuration) :-
    component(pytorch_extension, test_coverage, Coverage),
    memberchk(unit_tests(pytest), Coverage),
    memberchk(gradient_verification(torch_autograd_gradcheck), Coverage),
    memberchk(performance_benchmarks(pytest_benchmark), Coverage).

% Test spell system constructors
test(spell_constructors) :-
    component(conjure, ctor, build_extension),
    component(conjure, ctor, run_tests),
    component(conjure, ctor, run_benchmarks),
    component(perceive, ctor, operation_info),
    component(perceive, ctor, performance_stats).

% Test perceive operation info spell
test(perceive_operation_info) :-
    perceive(operation_info(parametric_swish), Info),
    memberchk(operation_type-activation_function, Info),
    memberchk(autograd_support-true, Info).

% Test perceive performance stats spell  
test(perceive_performance_stats) :-
    perceive(performance_stats(parametric_swish), Stats),
    Stats = stats(profile(_), complexity(o_n)).

% Test conjure spells (simulated)
test(conjure_build_extension) :-
    cast(conjure(build_extension), Result),
    Result = extension_built_successfully.

test(conjure_run_tests) :-
    cast(conjure(run_tests), Result),
    Result = test_result(passed(all_tests)).

test(conjure_run_benchmarks) :-
    cast(conjure(run_benchmarks), Result),
    Result = benchmark_result(completed(performance_data)).

% Test docstrings exist
test(docstrings_exist) :-
    docstring(pytorch_extension, Doc1),
    atom_string(Doc1, DocStr1),
    sub_string(DocStr1, _, _, _, "PyTorch C++ Extension Template"),
    
    docstring(parametric_swish, Doc2),
    atom_string(Doc2, DocStr2),
    sub_string(DocStr2, _, _, _, "Parametric Swish activation function"),
    
    docstring(fused_attention, Doc3),
    atom_string(Doc3, DocStr3),
    sub_string(DocStr3, _, _, _, "Fused attention operation").

% Test memory usage patterns
test(memory_usage_patterns) :-
    component(parametric_swish, memory_usage, in_place_eligible),
    component(fused_attention, memory_usage, reduced_intermediate_storage).

% Test nix integration
test(nix_integration) :-
    component(pytorch_extension, nix_integration, NixConfig),
    memberchk(flake_based, NixConfig),
    memberchk(dev_shell_provided, NixConfig).

% Test fusion operations for fused attention
test(fusion_operations) :-
    component(fused_attention, fusion_operations, Operations),
    memberchk(matmul, Operations),
    memberchk(softmax, Operations),
    memberchk(weighted_sum, Operations).

:- end_tests(pytorch_extension).

% Test runner
run_tests :-
    run_tests(pytorch_extension).