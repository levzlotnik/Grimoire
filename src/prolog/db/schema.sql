CREATE TABLE IF NOT EXISTS sessions (
    id INTEGER PRIMARY KEY,
    git_sha TEXT,                -- Git context at session start
    git_branch TEXT,            -- Branch name for better context
    start_time DATETIME DEFAULT CURRENT_TIMESTAMP,
    end_time DATETIME,          -- NULL until session ends
    project_path TEXT NOT NULL  -- Absolute path to project root
);

CREATE TABLE IF NOT EXISTS thoughts (
    id INTEGER PRIMARY KEY,
    session_id INTEGER NOT NULL,
    step_number INTEGER NOT NULL,
    thought_type TEXT NOT NULL CHECK(
        thought_type IN ('user_input', 'natural_language', 'tool_call', 'tool_result', 'return_value')
    ),
    -- Common fields
    content TEXT NOT NULL,         -- Main content (prompt/reasoning/result)
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
    parent_thought_id INTEGER,
    -- Tool-specific fields
    tool_name TEXT,               -- NULL unless thought_type = 'tool_call'
    tool_parameters TEXT,         -- JSON, NULL unless thought_type = 'tool_call'
    tool_reason TEXT,            -- NULL unless thought_type = 'tool_call'
    tool_result TEXT,            -- JSON, NULL unless thought_type = 'tool_result'
    tool_error TEXT,             -- NULL unless thought_type = 'tool_result' and error occurred
    -- Constraints
    FOREIGN KEY(session_id) REFERENCES sessions(id),
    FOREIGN KEY(parent_thought_id) REFERENCES thoughts(id),
    -- Type safety constraints
    CHECK (
        (thought_type = 'tool_call' AND tool_name IS NOT NULL AND tool_parameters IS NOT NULL AND tool_reason IS NOT NULL)
        OR (thought_type = 'tool_result' AND tool_result IS NOT NULL)
        OR (thought_type NOT IN ('tool_call', 'tool_result') AND
            tool_name IS NULL AND tool_parameters IS NULL AND
            tool_reason IS NULL AND tool_result IS NULL AND
            tool_error IS NULL)
    )
);

CREATE TABLE IF NOT EXISTS transactions (
    id INTEGER PRIMARY KEY,
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
