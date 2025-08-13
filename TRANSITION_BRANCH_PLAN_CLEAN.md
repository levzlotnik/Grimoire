# Transition Branch System - Implementation Plan

## Overview
Implement a "transition branch" system to safely handle unstaged changes when creating new sessions, ensuring Git state integrity and providing full audit trails.

## Current Problem
- Session creation with unstaged changes can lose work or corrupt Git state
- Stashing is temporary and doesn't provide audit trail
- Direct branch creation with dirty working tree can fail

## Proposed Solution: Transition Branches

### Flow Pattern - NEW SESSIONS ONLY
```
Source State (dirty) -> Transition Branch -> New Target Session
main (dirty) -> transition_branch/main--session-uuid -> session-uuid (based on transition)
```

### Key Principle: Transitions Only for New Sessions
- **New session creation**: Use transition branch to preserve unstaged changes
- **Existing session switching**: Git prevents incompatible switches as intended
- **Transition branches**: Created when target session doesn't exist
- **Transition lifecycle**: Provide stable base for session branch, deleted when session merges to main

### Detailed Flow
1. **Check current git status for tracked unstaged changes**
2. **If clean**: Direct session creation
3. **If dirty**:
   - Create transition branch from source
   - Commit tracked unstaged changes to transition
   - Create new session branch based on transition branch

### Branch Naming Convention
- Sessions: `session-{uuid}`
- Transitions: `transition_branch/{source}--{target}`
- Examples:
  - `transition_branch/main--session-abc123` (main to new session)
  - `transition_branch/session-abc123--session-def456` (session to new session)

## Implementation Plan

### Phase 1: Core Transition Infrastructure

#### 1.1 Git Status Check
- `check_tracked_changes/1` - detect tracked unstaged changes
- `has_dirty_tracked_files/0` - simple dirty state check
- `should_use_transition/1` - decide transition vs direct creation

#### 1.2 Transition Branch Creation (New Sessions Only)
- `create_transition_for_new_session/3` - create transition branch with tracked changes
- `commit_tracked_changes_only/2` - commit tracked unstaged files
- `create_session_from_transition/3` - create new session based on transition

#### 1.3 Enhanced Session Creation
- `start_session_with_transition/2` - session creation with transition support
- `direct_session_creation/2` - clean state session creation
- `transition_based_session_creation/2` - dirty state session creation

### Phase 2: Basic Error Handling

#### 2.1 Git Operation Failures
- Handle `git checkout -b` failures (rare - disk space, permissions)
- Handle `git commit` failures (empty changes - skip transition)
- Handle branch name conflicts (add timestamp suffix)

### Phase 3: Transition Branch Management

#### 3.1 Listing and Discovery
- `list_transition_branches/1` - find all transition branches
- `find_transition_for_session/2` - get transition branch for specific session
- `get_transition_history/2` - audit trail for session creation

#### 3.2 Immediate Cleanup on Merge
- `delete_transition_on_merge/2` - remove transition when session merges to main
- `cleanup_orphaned_transitions/1` - remove transitions for deleted sessions
- `verify_transition_integrity/1` - validate transition branch relationships

### Phase 4: Integration and Safety

#### 4.1 Update Existing Operations
- Modify `close_session/3` to delete transition branch when merging to main
- Update session merge operations with automatic transition cleanup
- Enhance session lifecycle with transition management

#### 4.2 Safety Enhancements
- Prevent transition branch conflicts
- Validate transition branch integrity
- Add transition branch protection

#### 4.3 Session Lifecycle Integration
- Track transitions in session stats
- Include transitions in session history
- Add transition info to session context

## Detailed Implementation Specs

### Data Structures

#### Transition Branch Info
```prolog
transition(
    branch_name(BranchName),
    source_branch(SourceBranch),
    target_session(TargetSessionId),
    created_at(Timestamp),
    commit_hash(CommitHash)
).
```

#### Status Results
```prolog
git_status(clean).
git_status(dirty(staged_changes, unstaged_changes, untracked_files)).
```

### Core Predicates

#### `start_session_with_transition/2`
```prolog
start_session_with_transition(SessionId, Result) :-
    % 1. Check current git status for tracked unstaged changes
    % 2. If clean: direct session creation
    % 3. If dirty: create transition branch, commit tracked changes, create session from transition
    % 4. Return detailed result with transition info
```

#### `create_transition_for_new_session/3`
```prolog
create_transition_for_new_session(SourceBranch, SessionId, Result) :-
    % 1. Generate transition branch name
    % 2. Create transition branch from source
    % 3. Stage tracked files with changes
    % 4. Commit tracked changes with descriptive message
    % 5. Return transition branch info for session creation
```

#### `delete_transition_on_merge/2`
```prolog
delete_transition_on_merge(SessionId, Result) :-
    % 1. Find transition branch for session
    % 2. Verify session has been merged to main
    % 3. Delete transition branch immediately
    % 4. Return cleanup result
```

### Error Scenarios and Handling

#### Git Operation Failures
- **Branch creation failure**: Disk space, permissions, invalid names - operation fails atomically
- **Empty commit**: Skip transition, proceed with direct session creation
- **Branch name conflict**: Add timestamp suffix to transition branch name

#### Simple Failure Handling
- **Failed branch creation**: Stay on source branch, return error
- **Failed commit**: Delete created branch, stay on source, return error
- **Failed session creation**: Transition branch exists as stable recovery point

## Testing Strategy

### Unit Tests
- Transition branch creation with various change types
- Status detection accuracy
- Branch name generation uniqueness
- Cleanup logic correctness

### Integration Tests
- Full session creation workflows
- Error recovery scenarios
- Cleanup operations under load
- Session lifecycle with transitions

### Safety Tests
- Data loss prevention
- Git repository integrity
- Concurrent operation safety
- Emergency recovery procedures

## Migration Plan

### Phase 1: Add Infrastructure (No Breaking Changes)
- Add transition predicates alongside existing ones
- Update session creation to use new system
- Maintain backward compatibility

### Phase 2: Enhanced Operations
- Update all session operations to use transitions
- Add transition management commands
- Comprehensive testing

### Phase 3: Optimization and Cleanup
- Remove legacy transition handling
- Optimize transition branch management
- Performance tuning

## Risk Assessment

### High Risk
- **Data loss during transition creation** - Mitigated by pre-flight checks
- **Git repository corruption** - Mitigated by atomic operations
- **Session state inconsistency** - Mitigated by verification steps

### Medium Risk
- **Orphaned transitions** - Mitigated by cleanup on session deletion
- **Complex error scenarios** - Mitigated by comprehensive testing

### Low Risk
- **Branch naming conflicts** - Handled by name generation logic
- **User confusion** - Mitigated by clear documentation
- **Legacy compatibility** - Handled by gradual migration

## Success Criteria

### Functional Requirements
✅ Safe session creation with tracked change preservation
✅ Stable base branches for session development
✅ Automatic transition cleanup on session merge
✅ Recovery from failed transitions
✅ Integration with existing session lifecycle

### Performance Requirements
✅ All operations are Git-native (inherently fast)
✅ Git handles optimization internally

### Safety Requirements
✅ Zero data loss scenarios
✅ Git repository integrity maintained
✅ Rollback capability for all operations
✅ Emergency recovery procedures

## Implementation Order

1. **Core Infrastructure** (start_session_with_transition, create_transition_for_new_session)
2. **Safety Mechanisms** (pre-flight checks, rollback capabilities)
3. **Management Operations** (cleanup, listing, discovery)
4. **Integration** (update existing predicates)
5. **Testing and Validation** (comprehensive test suite)
6. **Documentation and Examples** (user guides, API docs)

---

**Next Steps**: Review this plan, identify any gaps or concerns, then proceed with Phase 1 implementation.
