"""
Grimoire API Package

A Python package for interfacing with the Grimoire knowledge-based OS.
Provides both library interface and executable servers (MCP and REST).
"""

from .grimoire_interface import (
    GrimoireInterface,
    GrimoireError,
    # Response models
    ComponentTypesResponse,
    ComponentsResponse, 
    ComponentEntry,
    DocumentationResponse,
    StatusResponse,
    StatusInfo,
    PerceiveResponse,
    ConjureResponse,
    SystemInfo,
    EntitiesResponse,
    TestResponse,
    SessionCommandResponse,
    LoadResponse,
)

__all__ = [
    # Main interface class
    'GrimoireInterface',
    'GrimoireError',
    # Response models
    'ComponentTypesResponse',
    'ComponentsResponse',
    'ComponentEntry', 
    'DocumentationResponse',
    'StatusResponse',
    'StatusInfo',
    'PerceiveResponse',
    'ConjureResponse',
    'SystemInfo',
    'EntitiesResponse',
    'TestResponse',
    'SessionCommandResponse',
    'LoadResponse',
]

__version__ = "0.1.0"
__author__ = "Grimoire"
__description__ = "Python interface to Grimoire knowledge-based OS"