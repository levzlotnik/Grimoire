"""
Grimoire Python API

Public exports for the grimoire package.
"""

from .client import (
    Grimoire,
    GrimoireError,
    FromPrologTerm,
    # Response models
    ComponentTypesResponse,
    ComponentsResponse,
    DocstringResponse,
    EntitiesResponse,
    TestResponse,
    SystemInstructionsResponse,
    ExecResponse,
    ConjureResponse,
    PerceiveResponse,
    GenericResponse,
    SessionContextResponse,
)

__all__ = [
    'Grimoire',
    'GrimoireError',
    'FromPrologTerm',
    'ComponentTypesResponse',
    'ComponentsResponse',
    'DocstringResponse',
    'EntitiesResponse',
    'TestResponse',
    'SystemInstructionsResponse',
    'ExecResponse',
    'ConjureResponse',
    'PerceiveResponse',
    'GenericResponse',
    'SessionContextResponse',
]

__version__ = "0.1.0"
