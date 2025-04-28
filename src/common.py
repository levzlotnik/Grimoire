import json
from typing import Callable, Dict, List, ParamSpec, Tuple, Type, TypeVar, Union
from functools import wraps

from pydantic import BaseModel


class ToolError(Exception):
    """Raised when a tool call fails"""

    pass


P = ParamSpec("P")
R = TypeVar("R")


def wrap_tool_error(exc_types: Tuple[Type[Exception]]):
    """Decorator factory:
    Wrap a function in a try-except block to raise a ToolError if an exception is raised.
    """

    def decorator(fn: Callable[P, R]) -> Callable[P, R]:
        @wraps(fn)  # This preserves the original function's metadata
        def wrapper(*args, **kwargs):
            try:
                return fn(*args, **kwargs)
            except exc_types as e:
                raise ToolError(str(e)) from e

        return wrapper

    return decorator


NoneType = type(None)
PrimType = Union[
    NoneType, bool, int, str, float, BaseModel, Dict[str, "PrimType"], List["PrimType"]
]


def dump_val(val: PrimType) -> str:
    """Dump the result to a string format"""
    if isinstance(val, BaseModel):
        return json.dumps(val.model_dump(), indent=2)
    elif isinstance(val, (str, int, float, NoneType)):
        return str(val)
    elif isinstance(val, dict):
        return json.dumps({k: dump_val(v) for k, v in val.items()}, indent=2)
    elif isinstance(val, list):
        return json.dumps([dump_val(item) for item in val], indent=2)
    else:
        raise TypeError(f"Unsupported result type: {type(val)}")
