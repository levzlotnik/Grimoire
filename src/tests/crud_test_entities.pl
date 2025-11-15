% Test entities for CRUD operations testing
:- self_entity(crud_test_entity_with_derived).

% Conditional expansion - generates derived component
component(crud_test_entity_with_derived, has(test_expansion), test_expansion(data(test_value)))
    ==> component(crud_test_entity_with_derived, test_derived_comp, test_value).

% Triggering fact - causes the ==> above to generate the derived component
component(crud_test_entity_with_derived, has(test_expansion), test_expansion(data(test_value))).
