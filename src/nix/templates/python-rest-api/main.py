from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import Optional, Any, Dict

app = FastAPI(title="REST API Template", version="0.1.0")

# Request/Response models
class PerceiveRequest(BaseModel):
    query: str
    session_id: Optional[str] = None

class ConjureRequest(BaseModel):
    spell: str
    session_id: Optional[str] = None

class DataRequest(BaseModel):
    data: Dict[str, Any]
    session_id: Optional[str] = None

class APIResponse(BaseModel):
    success: bool
    result: Any
    session_id: Optional[str] = None

# Example endpoints
@app.get("/")
async def root():
    """Root endpoint"""
    return {"message": "REST API Template is running"}

@app.get("/hello")
async def hello():
    """GET endpoint example"""
    return {"message": "Hello from REST API!"}

@app.post("/data")
async def process_data(request: DataRequest):
    """POST endpoint example that processes data"""
    # Process the data (example transformation)
    processed = {
        "received": request.data,
        "processed": True,
        "keys": list(request.data.keys()),
        "session_id": request.session_id
    }
    
    return APIResponse(
        success=True,
        result=processed,
        session_id=request.session_id
    )

@app.get("/perceive")
async def perceive(query: str, session_id: Optional[str] = None):
    """Perception query endpoint"""
    # Placeholder for perception logic
    return APIResponse(
        success=True,
        result={"query": query, "answer": "Perception result placeholder"},
        session_id=session_id
    )

@app.post("/conjure")
async def conjure(request: ConjureRequest):
    """Conjuration spell endpoint"""
    # Placeholder for conjuration logic
    return APIResponse(
        success=True,
        result={"spell": request.spell, "effect": "Conjuration executed"},
        session_id=request.session_id
    )

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {"status": "healthy"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)