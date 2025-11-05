% Test entity for git domain tests
% This file contains declarative entity/component definitions for testing

:- self_entity(test_entity(git)).

% Test entities for different scenarios
entity(test_project).
entity(git_mock_entity).
entity(git_bad_entity).
entity(git_not_git_entity).
entity(git_complete_test).

% Git repository component for test project with DSL pattern
component(test_project, has(git(repository)), git(repository([
    remote(origin, 'https://github.com/test/repo'),
    branch(main),
    clean(true)
]))).

% Test project needs a self component for git_repository_root derivation
component(test_project, self, semantic(folder('/tmp/test_project'))).
component(test_project, core_dump_ignorelist, [git_repository_root]).

% Bad entity with nonexistent path (for negative test)
component(git_bad_entity, git_repository_root, '/nonexistent/path').
component(git_bad_entity, core_dump_ignorelist, [git_repository_root]).

% Mock entity with predictable test path
component(git_mock_entity, git_repository_root, '/tmp/test_git_mock').
component(git_mock_entity, core_dump_ignorelist, [git_repository_root]).

% Not-git entity with predictable test path (directory without .git)
component(git_not_git_entity, git_repository_root, '/tmp/test_not_git').
component(git_not_git_entity, core_dump_ignorelist, [git_repository_root]).

% Complete test entity with full DSL pattern
component(git_complete_test, has(git(repository)), git(repository([
    remote(origin, 'https://github.com/test/complete_repo'),
    branch(main)
]))).
component(git_complete_test, self, semantic(folder('/tmp/test_git_complete'))).
component(git_complete_test, core_dump_ignorelist, [git_repository_root]).

% Skill test entities
entity(git_skill_test).
component(git_skill_test, self, semantic(folder('/tmp/test_git_skills'))).
component(git_skill_test, has(git(repository)), git(repository([
    branch(main),
    clean(true)
]))).
component(git_skill_test, core_dump_ignorelist, [git_repository_root]).

entity(git_skill_remote_test).
component(git_skill_remote_test, self, semantic(folder('/tmp/test_git_skills_remote'))).
component(git_skill_remote_test, has(git(repository)), git(repository([
    branch(main),
    remote(origin, 'https://example.com/repo.git'),
    clean(true)
]))).
component(git_skill_remote_test, core_dump_ignorelist, [git_repository_root]).

docstring(test_entity(git), "Test entity container for git domain verification tests").
docstring(test_project, "Test project entity with git repository").
docstring(git_skill_test, "Test entity for git skill derivation without remote").
docstring(git_skill_remote_test, "Test entity for git skill derivation with remote").
