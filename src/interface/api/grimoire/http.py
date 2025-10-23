"""
Grimoire REST API - FastAPI-based HTTP interface

Provides RESTful API endpoints for accessing Grimoire functionality over HTTP.
Uses new Grimoire client with typed interface operations.
"""

from fastapi import FastAPI, HTTPException, Query, Path
from pydantic import BaseModel
from typing import Optional, Any, List, Dict

from grimoire.client import Grimoire, GrimoireError

# Create global instance of Grimoire interface
grimoire = Grimoire()

app = FastAPI(
    title="Grimoire Interface API",
    version="0.2.0",
    description="FastAPI-based HTTP interface for Grimoire knowledge-based OS. Uses janus-swi for direct Prolog integration."
)


# Request/Response models
class PerceiveRequest(BaseModel):
    query_sig: str
    args: Optional[Dict[str, Any]] = None


class ConjureRequest(BaseModel):
    spell_sig: str
    args: Optional[Dict[str, Any]] = None


class APIResponse(BaseModel):
    success: bool
    result: Any
    error: Optional[str] = None


# Root endpoint
@app.get("/")
async def root():
    """Root endpoint - returns API information"""
    try:
        system_info = {
            "api": "Grimoire Interface API",
            "version": "0.2.0",
            "description": "HTTP access to Grimoire interface functionality"
        }

        endpoints = [
            {"method": "GET", "path": "/component_types/{entity}", "description": "List component types"},
            {"method": "GET", "path": "/components/{entity}/{type}", "description": "Get components"},
            {"method": "GET", "path": "/docstring/{entity}", "description": "Get docstring"},
            {"method": "GET", "path": "/entities", "description": "List all entities"},
            {"method": "GET", "path": "/spells", "description": "List all spells"},
            {"method": "POST", "path": "/perceive", "description": "Execute perception query"},
            {"method": "POST", "path": "/conjure", "description": "Execute conjuration spell"},
            {"method": "GET", "path": "/test", "description": "Run tests"},
            {"method": "GET", "path": "/health", "description": "Health check"}
        ]

        return {
            "system_info": system_info,
            "endpoints": endpoints
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# Component types endpoint
@app.get("/component_types/{entity}")
async def get_component_types(entity: str = Path(...)):
    """List all component types for an entity"""
    try:
        result = grimoire.component_types(entity)
        return {"success": True, "result": result.model_dump()}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Components endpoint
@app.get("/components/{entity}/{comp_type}")
async def get_components(
    entity: str = Path(...),
    comp_type: str = Path(...)
):
    """Get verified components with smart singleton/set detection"""
    try:
        result = grimoire.components(entity, comp_type)
        return {"success": True, "result": result.model_dump()}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Docstring endpoint
@app.get("/docstring/{entity}")
async def get_docstring(entity: str = Path(...)):
    """Get entity docstring"""
    try:
        result = grimoire.docstring(entity)
        return {"success": True, "result": {"docstring": result.docstring}}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Entities endpoint
@app.get("/entities")
async def get_entities():
    """List all entities in the system"""
    try:
        result = grimoire.entities()
        return {"success": True, "result": result.model_dump()}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Spells endpoint
@app.get("/spells")
async def list_spells():
    """List all registered spells"""
    try:
        spells = grimoire.list_all_spells()
        return {"success": True, "result": {"spells": spells}}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Perception endpoint
@app.post("/perceive")
async def perceive_query(request: PerceiveRequest):
    """Execute perception query"""
    try:
        result = grimoire.perceive(request.query_sig, request.args or {})
        return {"success": True, "result": {"output": result.result}}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Conjuration endpoint
@app.post("/conjure")
async def conjure_spell(request: ConjureRequest):
    """Execute conjuration spell"""
    try:
        result = grimoire.conjure(request.spell_sig, request.args or {})
        return {"success": True, "result": {"output": result.result}}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Test endpoint
@app.get("/test")
async def run_tests(test_names: Optional[List[str]] = Query(None)):
    """Run test suite"""
    try:
        result = grimoire.test(test_names)
        return {"success": True, "result": result.model_dump()}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Prove it endpoint
@app.get("/prove_it/{entity}/{comp_type}")
async def prove_it(
    entity: str = Path(...),
    comp_type: str = Path(...),
    value: str = Query(...)
):
    """Component provenance - where generated and how verified"""
    try:
        result = grimoire.prove_it(entity, comp_type, value)
        return {"success": True, "result": {"output": result.result}}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Sauce me endpoint
@app.get("/sauce_me/{spell_ctor}")
async def sauce_me(spell_ctor: str = Path(...)):
    """Spell metadata - source location, implementation, formats"""
    try:
        result = grimoire.sauce_me(spell_ctor)
        return {"success": True, "result": {"output": result.result}}
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")


# Health check endpoint
@app.get("/health")
async def health_check():
    """Health check endpoint"""
    try:
        # Test basic connectivity
        entities = grimoire.entities()
        return {
            "status": "healthy",
            "api_version": "0.2.0",
            "entity_count": len(entities.entities)
        }
    except Exception as e:
        return {
            "status": "unhealthy",
            "api_version": "0.2.0",
            "error": str(e)
        }


def main(argv: Optional[List[str]] = None) -> None:
    """Main entry point for grimoire-rest-api server

    Args:
        argv: Command line arguments. If None, uses sys.argv[1:]
    """
    import argparse
    import uvicorn
    import sys

    if argv is None:
        argv = sys.argv[1:]

    parser = argparse.ArgumentParser(description="Grimoire Interface API Server")
    parser.add_argument("--host", default="0.0.0.0", help="Host to bind to (default: 0.0.0.0)")
    parser.add_argument("--port", type=int, default=8000, help="Port to bind to (default: 8000)")
    parser.add_argument("--reload", action="store_true", help="Enable auto-reload during development")

    args = parser.parse_args(argv)

    uvicorn.run(
        app,
        host=args.host,
        port=args.port,
        reload=args.reload
    )


if __name__ == "__main__":
    main()
