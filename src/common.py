from typing import Callable, List, Tuple, Type
from functools import wraps


class ToolError(Exception):
    """Raised when a tool call fails"""

    pass


def wrap_tool_error(exc_types: Tuple[Type[Exception]]):
    """Decorator factory:
    Wrap a function in a try-except block to raise a ToolError if an exception is raised.
    """

    def decorator(fn: Callable) -> Callable:
        @wraps(fn)  # This preserves the original function's metadata
        def wrapper(*args, **kwargs):
            try:
                return fn(*args, **kwargs)
            except exc_types as e:
                raise ToolError(str(e)) from e

        return wrapper

    return decorator
