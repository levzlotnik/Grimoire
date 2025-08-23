from fastapi import FastAPI, HTTPException, Query, Path
from pydantic import BaseModel
from typing import Optional, Any

# Import our Grimoire interface module
from grimoire_interface import GrimoireInterface, SystemInfo, InterfaceEndpoint, RootResponse

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
        InterfaceEndpoint(method="POST", path="/perceive", description=docstrings.get("perceive", "Execute perception spells")),
        InterfaceEndpoint(method="POST", path="/conjure", description=docstrings.get("conjure", "Execute conjuration spells")),
        InterfaceEndpoint(method="GET", path="/status", description=docstrings.get("status", "Show session status")),
        InterfaceEndpoint(method="GET", path="/health", description="Health check")
    ]
    
    system_info = grimoire.get_system_info()
    
    return RootResponse(
        success=True,
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
    result = grimoire.compt()
    return result

@app.get("/compt/{entity}")
async def list_entity_component_types(
    entity: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """List component types for specific entity → interface(compt(Entity))"""
    result = grimoire.compt(entity)
    return result

# Components endpoint
@app.get("/comp/{entity}/{comp_type}")
async def list_components(
    entity: str = Path(...),
    comp_type: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """List components of specific type for entity → interface(comp(Entity, Type))"""
    result = grimoire.comp(entity, comp_type)
    return result

# Documentation endpoints
@app.get("/doc")
async def show_documentation(session_id: Optional[str] = Query(None)):
    """Show system documentation → interface(doc)"""
    result = grimoire.doc()
    return result

@app.get("/doc/{entity}")
async def show_entity_documentation(
    entity: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """Show documentation for specific entity → interface(doc(Entity))"""
    result = grimoire.doc(entity)
    return result

# Perception endpoint
@app.post("/perceive")
async def perceive(request: PerceiveRequest):
    """Execute perception query → perceive(Query)"""
    result = grimoire.call_perceive_query(request.query)
    return result

# Conjuration endpoint
@app.post("/conjure")
async def conjure(request: ConjureRequest):
    """Execute conjuration spell → cast(conjure(Spell), Result)"""
    result = grimoire.call_conjure_spell(request.spell)
    return result

# Status endpoint
@app.get("/status")
async def get_status(session_id: Optional[str] = Query(None)):
    """Get session and system status → interface(status)"""
    result = grimoire.status()
    return result

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
            **system_info.dict()
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