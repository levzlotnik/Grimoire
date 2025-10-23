"""
Grimoire Python API

Public exports for the grimoire package.
"""

from .client import (
    Grimoire,
    GrimoireError,
    PrologTerm,
    PrologTermRecord,
    # Response models
    ComponentTypesResponse,
    ComponentsResponse,
    DocstringResponse,
    EntitiesResponse,
    TestResponse,
    SystemInstructionsResponse,
    ConjureResponse,
    PerceiveResponse,
    GenericResponse,
)

__all__ = [
    'Grimoire',
    'GrimoireError',
    'PrologTerm',
    'PrologTermRecord',
    'ComponentTypesResponse',
    'ComponentsResponse',
    'DocstringResponse',
    'EntitiesResponse',
    'TestResponse',
    'SystemInstructionsResponse',
    'ConjureResponse',
    'PerceiveResponse',
    'GenericResponse',
]

__version__ = "0.1.0"
