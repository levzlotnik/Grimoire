import json
import sqlite3
import logging
import git
import os
from abc import ABC, abstractmethod
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Union
from contextlib import contextmanager

from tool_calling import NaturalLanguage, ToolCall, RetVal, Thought


class SessionContext:
    """Represents a session's context"""

    @staticmethod
    def get_git_info(path: Path) -> tuple[Optional[str], Optional[str]]:
        """Get git SHA and branch for a path"""
        try:
            repo = git.Repo(str(path), search_parent_directories=True)
            try:
                branch = repo.active_branch.name
            except TypeError:
                branch = None  # Detached HEAD state
            return repo.head.commit.hexsha, branch
        except (git.InvalidGitRepositoryError, git.NoSuchPathError):
            return None, None

    def __init__(
        self,
        project_path: Path,
        git_sha: Optional[str] = None,
        git_branch: Optional[str] = None,
    ):
        self.project_path = project_path
        # Auto-capture git context if not provided
        if git_sha is None or git_branch is None:
            auto_sha, auto_branch = self.get_git_info(project_path)
            self.git_sha = git_sha or auto_sha
            self.git_branch = git_branch or auto_branch
        else:
            self.git_sha = git_sha
            self.git_branch = git_branch
        self.session_id: Optional[int] = None
        self.start_time = datetime.now()
        self.end_time: Optional[datetime] = None


class BaseSessionLogger(ABC):
    """Abstract base class for session loggers"""

    @abstractmethod
    def start_session(self, context: SessionContext) -> None:
        """Start a new session"""
        pass

    @abstractmethod
    def end_session(self) -> None:
        """End the current session"""
        pass

    @abstractmethod
    def log_thought(
        self, thought: Thought, parent_id: Optional[int] = None
    ) -> Optional[int]:
        """Log a thought and return its ID"""
        pass

    @abstractmethod
    def log_transaction(
        self, thought_id: int, commands: List[str], result: Union[str, Dict]
    ) -> None:
        """Log a transaction execution"""
        pass


class SQLiteSessionLogger(BaseSessionLogger):
    def __init__(self, db_path: Path):
        self.db_path = db_path
        self.conn = sqlite3.connect(str(db_path))
        self.conn.row_factory = sqlite3.Row

        # Initialize schema from environment variable
        schema_path = os.getenv("LLM_DB_SCHEMA_PATH")
        if not schema_path:
            raise RuntimeError("LLM_DB_SCHEMA_PATH environment variable not set")

        with open(schema_path) as f:
            with self.conn:
                self.conn.executescript(f.read())

        self.current_session: Optional[SessionContext] = None

    def start_session(self, context: SessionContext) -> None:
        with self.conn:
            cur = self.conn.execute(
                """
                INSERT INTO sessions (project_path, git_sha, git_branch, start_time)
                VALUES (:project_path, :git_sha, :git_branch, :start_time)
                RETURNING id
            """,
                {
                    "project_path": str(context.project_path),
                    "git_sha": context.git_sha,
                    "git_branch": context.git_branch,
                    "start_time": context.start_time,
                },
            )
            last_row = list(cur)[-1]
            session_id = last_row["id"]
            context.session_id = session_id
            self.current_session = context

    def end_session(self) -> None:
        if not self.current_session or not self.current_session.session_id:
            return

        self.current_session.end_time = datetime.now()
        with self.conn:
            self.conn.execute(
                "UPDATE sessions SET end_time = :end_time WHERE id = :id",
                {
                    "end_time": self.current_session.end_time,
                    "id": self.current_session.session_id,
                },
            )
            self.current_session = None

    def log_thought(self, thought: Thought, parent_id: Optional[int] = None):
        if self.current_session is None or self.current_session.session_id is None:
            raise RuntimeError("Session not started or session ID not set.")

        data = thought.data
        thought_type = data.tag
        content = data.model_dump_json()

        thought_fields = {
            "tool_name": None,
            "tool_parameters": None,
            "tool_reason": None,
            "retval": None,
            "feedback_message": None,
            "feedback_source": None,
            "feedback_success": None,
        }

        if thought_type == "tool_call":
            thought_fields["tool_name"] = data.tool_name
            thought_fields["tool_parameters"] = json.dumps(data.parameters)
            thought_fields["tool_reason"] = data.reason
        elif thought_type == "return":
            thought_fields["retval"] = data.retval
        elif thought_type == "feedback":
            thought_fields["feedback_message"] = data.message
            thought_fields["feedback_source"] = data.source
            thought_fields["feedback_success"] = int(data.success)
        # NaturalLanguage: just store content, all tool fields/retval/feedback remain None

        with self.conn:
            cur = self.conn.execute(
                """
                INSERT INTO thoughts (
                    session_id, step_number, thought_type, content,
                    parent_thought_id, tool_name, tool_parameters,
                    tool_reason, retval, agent_id,
                    feedback_message, feedback_source, feedback_success
                ) VALUES (
                    :session_id, :step_number, :thought_type, :content,
                    :parent_thought_id, :tool_name, :tool_parameters,
                    :tool_reason, :retval, :agent_id,
                    :feedback_message, :feedback_source, :feedback_success
                )
                RETURNING id
                """,
                {
                    "session_id": self.current_session.session_id,
                    "step_number": thought.step_number,
                    "thought_type": thought_type,
                    "content": content,
                    "parent_thought_id": parent_id,
                    "tool_name": thought_fields["tool_name"],
                    "tool_parameters": thought_fields["tool_parameters"],
                    "tool_reason": thought_fields["tool_reason"],
                    "retval": thought_fields["retval"],
                    "agent_id": thought.agent_id,
                    "feedback_message": thought_fields["feedback_message"],
                    "feedback_source": thought_fields["feedback_source"],
                    "feedback_success": thought_fields["feedback_success"],
                },
            )
            last_row = list(cur)[-1]
            thought_id = last_row["id"]
            return thought_id

    def log_transaction(
        self, thought_id: int, commands: List[str], result: Union[str, Dict]
    ) -> None:
        if not self.current_session:
            return

        status = (
            "success"
            if not isinstance(result, dict) or "error" not in result
            else "error"
        )
        result_json = json.dumps(result)
        commands_json = json.dumps(commands)

        with self.conn:
            self.conn.execute(
                """
                INSERT INTO transactions (thought_id, status, command_list, result)
                VALUES (?, ?, ?, ?)
            """,
                (thought_id, status, commands_json, result_json),
            )


class FileSessionLogger(BaseSessionLogger):
    def __init__(self, log_path: Path):
        self.logger = logging.getLogger("mypaos.session")
        self.logger.setLevel(logging.DEBUG)

        handler = logging.FileHandler(str(log_path))
        handler.setFormatter(
            logging.Formatter(
                "%(asctime)s - %(levelname)s - [%(session_id)s] %(message)s"
            )
        )
        self.logger.addHandler(handler)
        self.current_session: Optional[SessionContext] = None

    def start_session(self, context: SessionContext) -> None:
        self.current_session = context
        self.logger.info(
            f"Starting session for project: {context.project_path}",
            extra={"session_id": "NEW"},
        )

    def end_session(self) -> None:
        if self.current_session:
            self.logger.info(
                f"Ending session for project: {self.current_session.project_path}",
                extra={"session_id": self.current_session.session_id or "UNKNOWN"},
            )
        self.current_session = None

    def log_thought(
        self, thought: Thought, parent_id: Optional[int] = None
    ) -> Optional[int]:
        if not self.current_session:
            return None

        data = thought.data
        extra = {"session_id": self.current_session.session_id or "UNKNOWN"}

        if isinstance(data, NaturalLanguage):
            self.logger.info(f"Reasoning: {data.natural_language}", extra=extra)
        elif isinstance(data, ToolCall):
            self.logger.info(
                f"Tool call: {data.tool_name}\nReason: {data.reason}\nParams: {data.parameters}",
                extra=extra,
            )
        elif isinstance(data, RetVal):
            self.logger.info(f"Conclusion: {data.retval}", extra=extra)

        return None  # File logger doesn't track IDs

    def log_transaction(
        self, thought_id: int, commands: List[str], result: Union[str, Dict]
    ) -> None:
        if not self.current_session:
            return

        extra = {"session_id": self.current_session.session_id or "UNKNOWN"}
        self.logger.info(
            f"Executing transaction:\nCommands: {commands}\nResult: {result}",
            extra=extra,
        )

    def info(self, message: str) -> None:
        if not self.current_session:
            return

        extra = {"session_id": self.current_session.session_id or "UNKNOWN"}
        self.logger.info(message, extra=extra)

    def debug(self, message: str) -> None:
        if not self.current_session:
            return

        extra = {"session_id": self.current_session.session_id or "UNKNOWN"}
        self.logger.debug(message, extra=extra)

    def error(self, message: str) -> None:
        if not self.current_session:
            return

        extra = {"session_id": self.current_session.session_id or "UNKNOWN"}
        self.logger.error(message, extra=extra)


class InteractionLogger(BaseSessionLogger):
    """Logs both structured data and debug information"""

    def __init__(self, project_path: Path):
        mypaos_dir = project_path / ".mypaos"
        mypaos_dir.mkdir(exist_ok=True, parents=True)

        self.db_logger = SQLiteSessionLogger(mypaos_dir / "agent.db")
        self.file_logger = FileSessionLogger(mypaos_dir / "agent.log")

    def start_session(self, context: SessionContext) -> None:
        self.db_logger.start_session(context)
        self.file_logger.start_session(context)

    def end_session(self) -> None:
        self.db_logger.end_session()
        self.file_logger.end_session()

    def log_thought(
        self, thought: Thought, parent_id: Optional[int] = None
    ) -> Optional[int]:
        thought_id = self.db_logger.log_thought(thought, parent_id)
        self.file_logger.log_thought(thought, parent_id)
        return thought_id

    def log_transaction(
        self, thought_id: int, commands: List[str], result: Union[str, Dict]
    ) -> None:
        self.db_logger.log_transaction(thought_id, commands, result)
        self.file_logger.log_transaction(thought_id, commands, result)

    def info(self, message: str) -> None:
        self.file_logger.info(message)

    def debug(self, message: str) -> None:
        self.file_logger.debug(message)

    def error(self, message: str) -> None:
        self.file_logger.error(message)


@contextmanager
def session_logging(
    project_path: Path, git_sha: Optional[str] = None, git_branch: Optional[str] = None
):
    """Context manager for session logging"""
    logger = InteractionLogger(project_path)
    context = SessionContext(project_path, git_sha, git_branch)
    try:
        logger.start_session(context)
        yield logger
    finally:
        logger.end_session()
