from fastapi import FastAPI, HTTPException, Query, Path
from pydantic import BaseModel
from typing import Optional, Any

# Import our Grimoire interface module
from grimoire_interface import GrimoireInterface

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
@app.get("/", response_model=APIResponse)
async def root():
    """Root endpoint - returns API information and available endpoints"""
    endpoints = [
        {"method": "GET", "path": "/", "description": "API information"},
        {"method": "GET", "path": "/compt", "description": "List component types"},
        {"method": "GET", "path": "/compt/{entity}", "description": "List component types for entity"},
        {"method": "GET", "path": "/comp/{entity}/{type}", "description": "List components of type"},
        {"method": "GET", "path": "/doc", "description": "Show documentation"},
        {"method": "GET", "path": "/doc/{entity}", "description": "Show entity documentation"},
        {"method": "GET", "path": "/perceive", "description": "Execute perception query"},
        {"method": "POST", "path": "/conjure", "description": "Execute conjuration spell"},
        {"method": "GET", "path": "/status", "description": "Session status"},
        {"method": "GET", "path": "/health", "description": "Health check"}
    ]
    
    system_info = grimoire.get_system_info()
    
    return APIResponse(
        success=True,
        result={
            "api": "Grimoire Interface API", 
            "version": "0.1.0",
            "description": "HTTP access to Grimoire interface functionality via janus-swi",
            "system_info": system_info,
            "endpoints": endpoints
        }
    )

# Component types endpoints
@app.get("/compt", response_model=APIResponse)
async def list_component_types(session_id: Optional[str] = Query(None)):
    """List all component types → interface(compt)"""
    result = grimoire.call_interface_predicate("compt")
    
    return APIResponse(
        success=result["success"],
        result=result.get("result"),
        error=result.get("error"),
        session_id=session_id
    )

@app.get("/compt/{entity}", response_model=APIResponse)
async def list_entity_component_types(
    entity: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """List component types for specific entity → interface(compt(Entity))"""
    result = grimoire.call_interface_predicate("compt", [entity])
    
    return APIResponse(
        success=result["success"],
        result=result.get("result"),
        error=result.get("error"),
        session_id=session_id
    )

# Components endpoint
@app.get("/comp/{entity}/{comp_type}", response_model=APIResponse)
async def list_components(
    entity: str = Path(...),
    comp_type: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """List components of specific type for entity → interface(comp(Entity, Type))"""
    result = grimoire.call_interface_predicate("comp", [entity, comp_type])
    
    return APIResponse(
        success=result["success"],
        result=result.get("result"),
        error=result.get("error"),
        session_id=session_id
    )

# Documentation endpoints
@app.get("/doc", response_model=APIResponse)
async def show_documentation(session_id: Optional[str] = Query(None)):
    """Show system documentation → interface(doc)"""
    result = grimoire.call_interface_predicate("doc")
    
    return APIResponse(
        success=result["success"],
        result=result.get("result"),
        error=result.get("error"),
        session_id=session_id
    )

@app.get("/doc/{entity}", response_model=APIResponse)
async def show_entity_documentation(
    entity: str = Path(...),
    session_id: Optional[str] = Query(None)
):
    """Show documentation for specific entity → interface(doc(Entity))"""
    result = grimoire.call_interface_predicate("doc", [entity])
    
    return APIResponse(
        success=result["success"],
        result=result.get("result"),
        error=result.get("error"),
        session_id=session_id
    )

# Perception endpoint
@app.get("/perceive", response_model=APIResponse)
async def perceive(
    query: str = Query(...),
    session_id: Optional[str] = Query(None)
):
    """Execute perception query → perceive(Query)"""
    result = grimoire.call_perceive_query(query)
    
    return APIResponse(
        success=result["success"],
        result=result.get("solutions") if result["success"] else None,
        error=result.get("error"),
        session_id=session_id
    )

# Conjuration endpoint
@app.post("/conjure", response_model=APIResponse)
async def conjure(request: ConjureRequest):
    """Execute conjuration spell → cast(conjure(Spell), Result)"""
    result = grimoire.call_conjure_spell(request.spell)
    
    return APIResponse(
        success=result["success"],
        result=result.get("result"),
        error=result.get("error"),
        session_id=request.session_id
    )

# Status endpoint
@app.get("/status", response_model=APIResponse)
async def get_status(session_id: Optional[str] = Query(None)):
    """Get session and system status → interface(status)"""
    result = grimoire.call_interface_predicate("status")
    
    return APIResponse(
        success=result["success"],
        result=result.get("result"),
        error=result.get("error"),
        session_id=session_id
    )

# Health check endpoint
@app.get("/health")
async def health_check():
    """Health check endpoint"""
    try:
        # Test Prolog connection
        test_result = grimoire.test_prolog_connection()
        system_info = grimoire.get_system_info()
        
        status = "healthy" if grimoire.is_available() else "degraded"
        if not test_result.get("janus_available", False):
            status = "unhealthy"
        
        return {
            "status": status,
            "api_version": "0.1.0",
            **test_result,
            **system_info
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
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)

if __name__ == "__main__":
    main()