CREATE TABLE IF NOT EXISTS sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    git_sha TEXT,                -- Git context at session start
    git_branch TEXT,            -- Branch name for better context
    start_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    end_time DATETIME,          -- NULL until session ends
    project_path TEXT NOT NULL  -- Absolute path to project root
);

CREATE TABLE IF NOT EXISTS thoughts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id INTEGER NOT NULL,
    step_number INTEGER NOT NULL,
    agent_id TEXT NOT NULL,      -- ID of the agent that generated this thought
    thought_type TEXT NOT NULL CHECK(
        thought_type IN ('natural_language', 'tool_call', 'feedback', 'return')
    ),
    -- Common fields
    content TEXT NOT NULL,         -- Main content (prompt/reasoning/result)
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    parent_thought_id INTEGER,
    -- Tool-specific fields
    tool_name TEXT,               -- NULL unless thought_type = 'tool_call'
    tool_parameters TEXT,         -- JSON, NULL unless thought_type = 'tool_call'
    tool_reason TEXT,            -- NULL unless thought_type = 'tool_call'
    -- Feedback-specific fields
    feedback_message TEXT,       -- Only set if thought_type = 'feedback'
    feedback_source TEXT,        -- Only set if thought_type = 'feedback'
    feedback_success BOOLEAN,    -- Only set if thought_type = 'feedback'
    -- Return-specific field
    retval TEXT,                 -- Only set if thought_type = 'return'
    -- Constraints
    FOREIGN KEY(session_id) REFERENCES sessions(id),
    FOREIGN KEY(parent_thought_id) REFERENCES thoughts(id),
    -- Type safety constraints
    CHECK (
        (thought_type = 'tool_call' AND tool_name IS NOT NULL AND tool_parameters IS NOT NULL AND tool_reason IS NOT NULL
            AND retval IS NULL AND feedback_message IS NULL AND feedback_source IS NULL AND feedback_success IS NULL)
        OR (thought_type = 'return' AND retval IS NOT NULL
            AND tool_name IS NULL AND tool_parameters IS NULL AND tool_reason IS NULL
            AND feedback_message IS NULL AND feedback_source IS NULL AND feedback_success IS NULL)
        OR (thought_type = 'natural_language' AND tool_name IS NULL AND tool_parameters IS NULL AND tool_reason IS NULL AND retval IS NULL
            AND feedback_message IS NULL AND feedback_source IS NULL AND feedback_success IS NULL)
        OR (thought_type = 'feedback' AND feedback_message IS NOT NULL AND feedback_source IS NOT NULL AND feedback_success IS NOT NULL
            AND tool_name IS NULL AND tool_parameters IS NULL AND tool_reason IS NULL AND retval IS NULL)
    )
);

CREATE TABLE IF NOT EXISTS transactions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    thought_id INTEGER NOT NULL,
    executed_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    status TEXT NOT NULL,        -- 'success' or 'error'
    command_list TEXT NOT NULL,  -- JSON array of commands
    result TEXT NOT NULL,        -- JSON result/error message
    FOREIGN KEY(thought_id) REFERENCES thoughts(id)
);

-- Index for efficient lookups
CREATE INDEX IF NOT EXISTS thoughts_session_idx ON thoughts(session_id);
CREATE INDEX IF NOT EXISTS thoughts_type_idx ON thoughts(thought_type);
CREATE INDEX IF NOT EXISTS thoughts_parent_idx ON thoughts(parent_thought_id);
CREATE INDEX IF NOT EXISTS sessions_project_idx ON sessions(project_path);
CREATE INDEX IF NOT EXISTS transactions_thought_idx ON transactions(thought_id);
