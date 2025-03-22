ensure_loaded("./core_rules.pl")
ensure_loaded("./git.pl")

% The most fundamental entity
entity(system).
% Fundamental concepts
component(system, concept, command).
component(system, concept, transaction).
component(system, concept, hardware).
