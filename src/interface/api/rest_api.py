from fastapi import FastAPI, HTTPException, Query, Path
from pydantic import BaseModel
from typing import Optional, Any, List, Dict, Union

# Import our Grimoire interface module
from grimoire_interface import (
    GrimoireInterface, GrimoireError, SystemInfo, InterfaceEndpoint, RootResponse,
    EntitiesResponse, TestResponse, SessionCommandResponse, LoadResponse,
    ReadFileResponse, EditFileResponse
)

# Create global instance of Grimoire interface
grimoire = GrimoireInterface()

app = FastAPI(
    title="Grimoire Interface API",
    version="0.1.0",
    description="FastAPI-based HTTP interface for Grimoire knowledge-based OS. Uses janus-swi for direct Prolog integration."
)

# Request/Response models
class PerceiveRequest(BaseModel):
    query: str
    session_id: Optional[str] = None

class ConjureRequest(BaseModel):
    spell: str
    session_id: Optional[str] = None

class APIResponse(BaseModel):
    success: bool
    result: Any
    session_id: Optional[str] = None
    error: Optional[str] = None

# Root endpoint
@app.get("/", response_model=RootResponse)
async def root():
    """Root endpoint - returns API information and available endpoints with Prolog docstrings"""
    
    # Query real docstrings from Prolog
    docstrings = grimoire.query_interface_docstrings()
    
    # Build endpoints with real descriptions from Prolog
    endpoints = [
        InterfaceEndpoint(method="GET", path="/", description="API information"),
        InterfaceEndpoint(method="GET", path="/compt", description=docstrings.get("compt", "List component types")),
        InterfaceEndpoint(method="GET", path="/compt/{entity}", description=docstrings.get("compt", "List component types") + " for specific entity"),
        InterfaceEndpoint(method="GET", path="/comp/{entity}/{type}", description=docstrings.get("comp", "List components") + " of specific type"),
        InterfaceEndpoint(method="GET", path="/doc", description=docstrings.get("doc", "Show documentation")),
        InterfaceEndpoint(method="GET", path="/doc/{entity}", description=docstrings.get("doc", "Show documentation") + " for specific entity"),
        InterfaceEndpoint(method="GET", path="/entities", description=docstrings.get("entities", "List all entities")),
        InterfaceEndpoint(method="POST", path="/perceive", description=docstrings.get("perceive", "Execute perception spells")),
        InterfaceEndpoint(method="POST", path="/conjure", description=docstrings.get("conjure", "Execute conjuration spells")),
        InterfaceEndpoint(method="GET", path="/status", description=docstrings.get("status", "Show session status")),
        InterfaceEndpoint(method="GET", path="/test", description=docstrings.get("test", "Run test suite")),
        InterfaceEndpoint(method="POST", path="/session", description=docstrings.get("session", "Session management")),
        InterfaceEndpoint(method="POST", path="/load", description=docstrings.get("load", "Load entity")),
        InterfaceEndpoint(method="GET", path="/read_file/{file_path}", description="Read file with line numbers"),
        InterfaceEndpoint(method="POST", path="/edit_file", description="Edit file with specified operations"),
        InterfaceEndpoint(method="GET", path="/health", description="Health check")
    ]
    
    system_info = grimoire.get_system_info()
    
    return RootResponse(
        api="Grimoire Interface API", 
        version="0.1.0",
        description="HTTP access to Grimoire interface functionality via janus-swi",
        system_info=system_info,
        endpoints=endpoints
    )

# Component types endpoints
@app.get("/compt")
async def list_component_types(session_id: Optional[str] = Query(None)):
    """List all component types → interface(compt)"""
    try:
        return grimoire.compt()
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

@app.get("/compt/{entity}")
async def list_entity_component_types(
    entity: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """List component types for specific entity → interface(compt(Entity))"""
    try:
        return grimoire.compt(entity)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Components endpoint
@app.get("/comp/{entity}/{comp_type}")
async def list_components(
    entity: str = Path(...),
    comp_type: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """List components of specific type for entity → interface(comp(Entity, Type))"""
    try:
        return grimoire.comp(entity, comp_type)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Documentation endpoints
@app.get("/doc")
async def show_documentation(session_id: Optional[str] = Query(None)):
    """Show system documentation → interface(doc)"""
    try:
        return grimoire.doc()
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

@app.get("/doc/{entity}")
async def show_entity_documentation(
    entity: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """Show documentation for specific entity → interface(doc(Entity))"""
    try:
        return grimoire.doc(entity)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Perception endpoint
@app.post("/perceive")
async def perceive(request: PerceiveRequest):
    """Execute perception query → perceive(Query)"""
    try:
        return grimoire.call_perceive_query(request.query)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Conjuration endpoint
@app.post("/conjure")
async def conjure(request: ConjureRequest):
    """Execute conjuration spell → cast(conjure(Spell), Result)"""
    try:
        return grimoire.call_conjure_spell(request.spell)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Status endpoint
@app.get("/status")
async def get_status(session_id: Optional[str] = Query(None)):
    """Get session and system status → interface(status)"""
    try:
        return grimoire.status()
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Entities endpoint
@app.get("/entities")
async def list_entities():
    """List all entities in the system → interface(entities)"""
    try:
        return grimoire.entities()
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Test endpoint
@app.get("/test")
async def run_tests(test_names: Optional[List[str]] = Query(None)):
    """Run test suite → interface(test)"""
    try:
        return grimoire.test(test_names)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Session endpoint
class SessionRequest(BaseModel):
    args: List[str]

@app.post("/session")
async def session_command(request: SessionRequest):
    """Execute session management command → interface(session(args))"""
    try:
        return grimoire.session(request.args)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Load endpoint
class LoadRequest(BaseModel):
    entity_spec: str

@app.post("/load")
async def load_entity(request: LoadRequest):
    """Load entity into current session → interface(load(entity_spec))"""
    try:
        return grimoire.load(request.entity_spec)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")

# Read file endpoint  
@app.get("/read_file/{file_path:path}", response_model=ReadFileResponse)
async def read_file_endpoint(
    file_path: str = Path(...),
    start: int = Query(1, description="Starting line number (1-based indexing)"),
    end: int = Query(10, description="Ending line number (1-based indexing)"),
    session_id: Optional[str] = Query(None)
):
    """Read lines from a file using 1-based indexing → interface(read_file(FilePath, Start, End))"""
    try:
        return grimoire.read_file(file_path, start, end)
    except GrimoireError as e:
        raise HTTPException(status_code=500, detail=f"Grimoire error: {e}")
    except ValueError as e:
        raise HTTPException(status_code=400, detail=f"Invalid parameters: {e}")

# Edit file endpoint
from grimoire_interface import EditInsert, EditDelete, EditReplace, EditAppend

class EditFileRequest(BaseModel):
    file_path: str
    edits: List[Dict[str, Any]]  # Accept dicts for backward compatibility

@app.post("/edit_file", response_model=EditFileResponse)
async def edit_file_endpoint(request: EditFileRequest):
    """Edit file with specified operations → conjure(edit_file(file(Path), Edits))"""
    # Convert dicts to typed models
    typed_edits = []
    for edit in request.edits:
        op = edit.get("operation", "")
        if op == "insert":
            typed_edits.append(EditInsert(**edit))
        elif op == "delete":
            typed_edits.append(EditDelete(**edit))
        elif op == "replace":
            typed_edits.append(EditReplace(**edit))
        elif op == "append":
            typed_edits.append(EditAppend(**edit))
    
    return grimoire.edit_file(request.file_path, typed_edits)

# Health check endpoint
@app.get("/health")
async def health_check():
    """Health check endpoint"""
    try:
        # Test Prolog connection
        test_result = grimoire.test_prolog_connection()
        system_info = grimoire.get_system_info()
        
        status = "healthy"
        if not test_result.get("janus_available", False):
            status = "unhealthy"
        
        return {
            "status": status,
            "api_version": "0.1.0",
            **test_result,
            **system_info.model_dump()
        }
    except Exception as e:
        return {
            "status": "unhealthy",
            "api_version": "0.1.0",
            "error": str(e),
            "janus_available": False
        }

def main():
    """Main entry point for grimoire-server script"""
    import argparse
    import uvicorn
    
    parser = argparse.ArgumentParser(description="Grimoire Interface API Server")
    parser.add_argument("--host", default="0.0.0.0", help="Host to bind to (default: 0.0.0.0)")
    parser.add_argument("--port", type=int, default=8000, help="Port to bind to (default: 8000)")
    parser.add_argument("--reload", action="store_true", help="Enable auto-reload during development")
    
    args = parser.parse_args()
    
    uvicorn.run(
        app,
        host=args.host,
        port=args.port,
        reload=args.reload
    )

if __name__ == "__main__":
    main()