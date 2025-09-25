from fastapi import FastAPI, HTTPException, Depends, status, WebSocket, WebSocketDisconnect
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from sqlalchemy import create_engine, Column, Integer, String, DateTime, Boolean, Text
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker, Session
from pydantic import BaseModel, Field
from passlib.context import CryptContext
from jose import jwt, JWTError
from datetime import datetime, timedelta
from typing import Optional, List, Dict, Any
import json
import os
import tempfile
from pathlib import Path

# Configuration
SECRET_KEY = os.getenv("SECRET_KEY", "your-secret-key-here-change-in-production")
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30

# Database setup - use temp directory for database
db_path = os.path.join(tempfile.gettempdir(), "tasks.db")
SQLALCHEMY_DATABASE_URL = f"sqlite:///{db_path}"
engine = create_engine(SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False})
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
Base = declarative_base()

# Password hashing
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
security = HTTPBearer()


# Database Models
class User(Base):
    __tablename__ = "users"

    id = Column(Integer, primary_key=True, index=True)
    username = Column(String, unique=True, index=True)
    email = Column(String, unique=True, index=True)
    hashed_password = Column(String)
    is_active = Column(Boolean, default=True)
    created_at = Column(DateTime, default=datetime.utcnow)


class Task(Base):
    __tablename__ = "tasks"

    id = Column(Integer, primary_key=True, index=True)
    title = Column(String, index=True)
    description = Column(Text)
    completed = Column(Boolean, default=False)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    user_id = Column(Integer, index=True)


# Create tables
Base.metadata.create_all(bind=engine)


# Pydantic Models
class UserCreate(BaseModel):
    username: str = Field(..., min_length=3, max_length=50)
    email: str = Field(..., pattern=r"^[^@]+@[^@]+\.[^@]+$")
    password: str = Field(..., min_length=6)


class UserResponse(BaseModel):
    id: int
    username: str
    email: str
    is_active: bool
    created_at: datetime

    class Config:
        from_attributes = True


class Token(BaseModel):
    access_token: str
    token_type: str


class TaskCreate(BaseModel):
    title: str = Field(..., min_length=1, max_length=200)
    description: Optional[str] = None


class TaskUpdate(BaseModel):
    title: Optional[str] = Field(None, min_length=1, max_length=200)
    description: Optional[str] = None
    completed: Optional[bool] = None


class TaskResponse(BaseModel):
    id: int
    title: str
    description: Optional[str]
    completed: bool
    created_at: datetime
    updated_at: datetime
    user_id: int

    class Config:
        from_attributes = True


class WebSocketMessage(BaseModel):
    type: str
    data: Dict[str, Any]


# FastAPI app
app = FastAPI(
    title="React FastAPI Template",
    description="A fullstack template with React frontend and FastAPI backend",
    version="1.0.0",
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3000", "http://localhost:5173"],  # React dev servers
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# Connection manager for WebSockets
class ConnectionManager:
    def __init__(self):
        self.active_connections: List[WebSocket] = []

    async def connect(self, websocket: WebSocket):
        await websocket.accept()
        self.active_connections.append(websocket)

    def disconnect(self, websocket: WebSocket):
        self.active_connections.remove(websocket)

    async def send_personal_message(self, message: str, websocket: WebSocket):
        await websocket.send_text(message)

    async def broadcast(self, message: str):
        for connection in self.active_connections:
            try:
                await connection.send_text(message)
            except:
                pass


manager = ConnectionManager()


# Dependency to get database session
def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


# Authentication utilities
def verify_password(plain_password: str, hashed_password: str) -> bool:
    return pwd_context.verify(plain_password, hashed_password)


def get_password_hash(password: str) -> str:
    return pwd_context.hash(password)


def create_access_token(data: dict, expires_delta: Optional[timedelta] = None):
    to_encode = data.copy()
    if expires_delta:
        expire = datetime.utcnow() + expires_delta
    else:
        expire = datetime.utcnow() + timedelta(minutes=15)
    to_encode.update({"exp": expire})
    encoded_jwt = jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)
    return encoded_jwt


def get_user_by_username(db: Session, username: str):
    return db.query(User).filter(User.username == username).first()


def get_user_by_email(db: Session, email: str):
    return db.query(User).filter(User.email == email).first()


def get_user(db: Session, user_id: int):
    return db.query(User).filter(User.id == user_id).first()


def create_user(db: Session, user: UserCreate):
    hashed_password = get_password_hash(user.password)
    db_user = User(username=user.username, email=user.email, hashed_password=hashed_password)
    db.add(db_user)
    db.commit()
    db.refresh(db_user)
    return db_user


async def get_current_user(
    credentials: HTTPAuthorizationCredentials = Depends(security), db: Session = Depends(get_db)
):
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )
    try:
        payload = jwt.decode(credentials.credentials, SECRET_KEY, algorithms=[ALGORITHM])
        username: str = payload.get("sub")
        if username is None:
            raise credentials_exception
    except JWTError:
        raise credentials_exception

    user = get_user_by_username(db, username=username)
    if user is None:
        raise credentials_exception
    return user


# Authentication endpoints
@app.post("/api/register", response_model=UserResponse)
async def register(user: UserCreate, db: Session = Depends(get_db)):
    # Check if user already exists
    db_user = get_user_by_username(db, username=user.username)
    if db_user:
        raise HTTPException(status_code=400, detail="Username already registered")

    db_user = get_user_by_email(db, email=user.email)
    if db_user:
        raise HTTPException(status_code=400, detail="Email already registered")

    return create_user(db=db, user=user)


@app.post("/api/login", response_model=Token)
async def login(username: str, password: str, db: Session = Depends(get_db)):
    user = get_user_by_username(db, username)
    if not user or not verify_password(password, user.hashed_password):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password",
            headers={"WWW-Authenticate": "Bearer"},
        )
    access_token_expires = timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    access_token = create_access_token(data={"sub": user.username}, expires_delta=access_token_expires)
    return {"access_token": access_token, "token_type": "bearer"}


@app.get("/api/me", response_model=UserResponse)
async def read_users_me(current_user: User = Depends(get_current_user)):
    return current_user


# Task CRUD endpoints
@app.get("/api/tasks", response_model=List[TaskResponse])
async def get_tasks(db: Session = Depends(get_db), current_user: User = Depends(get_current_user)):
    tasks = db.query(Task).filter(Task.user_id == current_user.id).all()
    return tasks


@app.post("/api/tasks", response_model=TaskResponse)
async def create_task(task: TaskCreate, db: Session = Depends(get_db), current_user: User = Depends(get_current_user)):
    db_task = Task(**task.dict(), user_id=current_user.id)
    db.add(db_task)
    db.commit()
    db.refresh(db_task)

    # Broadcast new task to all connected clients
    await manager.broadcast(
        json.dumps(
            {"type": "task_created", "data": {"id": db_task.id, "title": db_task.title, "user": current_user.username}}
        )
    )

    return db_task


@app.get("/api/tasks/{task_id}", response_model=TaskResponse)
async def get_task(task_id: int, db: Session = Depends(get_db), current_user: User = Depends(get_current_user)):
    task = db.query(Task).filter(Task.id == task_id, Task.user_id == current_user.id).first()
    if not task:
        raise HTTPException(status_code=404, detail="Task not found")
    return task


@app.put("/api/tasks/{task_id}", response_model=TaskResponse)
async def update_task(
    task_id: int, task_update: TaskUpdate, db: Session = Depends(get_db), current_user: User = Depends(get_current_user)
):
    task = db.query(Task).filter(Task.id == task_id, Task.user_id == current_user.id).first()
    if not task:
        raise HTTPException(status_code=404, detail="Task not found")

    update_data = task_update.dict(exclude_unset=True)
    for field, value in update_data.items():
        setattr(task, field, value)

    task.updated_at = datetime.utcnow()
    db.commit()
    db.refresh(task)

    # Broadcast task update
    await manager.broadcast(
        json.dumps(
            {
                "type": "task_updated",
                "data": {
                    "id": task.id,
                    "title": task.title,
                    "completed": task.completed,
                    "user": current_user.username,
                },
            }
        )
    )

    return task


@app.delete("/api/tasks/{task_id}")
async def delete_task(task_id: int, db: Session = Depends(get_db), current_user: User = Depends(get_current_user)):
    task = db.query(Task).filter(Task.id == task_id, Task.user_id == current_user.id).first()
    if not task:
        raise HTTPException(status_code=404, detail="Task not found")

    db.delete(task)
    db.commit()

    # Broadcast task deletion
    await manager.broadcast(
        json.dumps({"type": "task_deleted", "data": {"id": task_id, "user": current_user.username}})
    )

    return {"message": "Task deleted successfully"}


# WebSocket endpoint
@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    await manager.connect(websocket)
    try:
        while True:
            data = await websocket.receive_text()
            try:
                message = json.loads(data)
                # Echo back the message to all connected clients
                await manager.broadcast(json.dumps({"type": "echo", "data": message}))
            except json.JSONDecodeError:
                await websocket.send_text(json.dumps({"type": "error", "data": {"message": "Invalid JSON format"}}))
    except WebSocketDisconnect:
        manager.disconnect(websocket)


# Dashboard data endpoints
@app.get("/api/dashboard/stats")
async def get_dashboard_stats():
    return {"total_users": 2847, "total_revenue": 24847, "total_orders": 847, "growth_rate": 3.2}


@app.get("/api/dashboard/monthly-data")
async def get_monthly_data():
    return [
        {"name": "Jan", "revenue": 2400, "expenses": 1800},
        {"name": "Feb", "revenue": 1398, "expenses": 1200},
        {"name": "Mar", "revenue": 9800, "expenses": 6500},
        {"name": "Apr", "revenue": 3908, "expenses": 2800},
        {"name": "May", "revenue": 4800, "expenses": 3200},
        {"name": "Jun", "revenue": 3800, "expenses": 2900},
    ]


@app.get("/api/dashboard/device-distribution")
async def get_device_distribution():
    return [
        {"name": "Desktop", "value": 400, "color": "#8884d8"},
        {"name": "Mobile", "value": 300, "color": "#82ca9d"},
        {"name": "Tablet", "value": 300, "color": "#ffc658"},
    ]


@app.get("/api/dashboard/hourly-activity")
async def get_hourly_activity():
    return [
        {"time": "00:00", "value": 20},
        {"time": "04:00", "value": 27},
        {"time": "08:00", "value": 35},
        {"time": "12:00", "value": 45},
        {"time": "16:00", "value": 38},
        {"time": "20:00", "value": 30},
    ]


@app.get("/api/dashboard/transactions")
async def get_recent_transactions():
    return [
        {"id": "#001", "user": "john@example.com", "amount": "$99.00", "status": "completed"},
        {"id": "#002", "user": "jane@example.com", "amount": "$149.00", "status": "pending"},
        {"id": "#003", "user": "bob@example.com", "amount": "$79.00", "status": "completed"},
    ]


# Health check
@app.get("/api/health")
async def health_check():
    return {"status": "healthy", "timestamp": datetime.utcnow()}


@app.get("/api/docs/openapi")
async def get_openapi_schema():
    """Get the OpenAPI schema with full endpoint details"""
    return app.openapi()


@app.get("/api/docs/endpoints")
async def get_api_endpoints():
    """Get all available API endpoints with their details from OpenAPI schema"""
    from fastapi.openapi.utils import get_openapi

    # Get the full OpenAPI schema
    openapi_schema = get_openapi(
        title=app.title,
        version=app.version,
        description=app.description,
        routes=app.routes,
    )

    endpoints = []

    # Parse the OpenAPI schema paths
    for path, methods in openapi_schema.get("paths", {}).items():
        # Skip non-API routes
        if not path.startswith("/api/"):
            continue

        for method, details in methods.items():
            if method.upper() in ["HEAD", "OPTIONS"]:
                continue

            endpoint_info = {
                "method": method.upper(),
                "path": path,
                "summary": details.get("summary", ""),
                "description": details.get("description", ""),
                "tags": details.get("tags", []),
                "parameters": [],
                "requestBody": None,
                "responses": details.get("responses", {}),
                "security": [sec for sec in details.get("security", []) if sec],
                "examples": {},
            }

            # Parse parameters
            for param in details.get("parameters", []):
                endpoint_info["parameters"].append(
                    {
                        "name": param.get("name"),
                        "in": param.get("in"),
                        "required": param.get("required", False),
                        "type": param.get("schema", {}).get("type", "string"),
                        "description": param.get("description", ""),
                        "example": param.get("example"),
                    }
                )

            # Parse request body
            if "requestBody" in details:
                req_body = details["requestBody"]
                content = req_body.get("content", {})
                if "application/json" in content:
                    endpoint_info["requestBody"] = {"description": req_body.get("description", ""), "content": content}

            # Generate realistic examples based on path
            if "/dashboard/stats" in path:
                endpoint_info["examples"] = {
                    "response": {"total_users": 2847, "total_revenue": 24847, "total_orders": 847, "growth_rate": 3.2}
                }
            elif "/dashboard/monthly-data" in path:
                endpoint_info["examples"] = {
                    "response": [
                        {"name": "Jan", "revenue": 2400, "expenses": 1800},
                        {"name": "Feb", "revenue": 1398, "expenses": 1200},
                    ]
                }

            endpoints.append(endpoint_info)

    # Sort by path for consistent ordering
    endpoints.sort(key=lambda x: (x["path"], x["method"]))

    return {
        "endpoints": endpoints,
        "total_count": len(endpoints),
        "generated_at": datetime.now().isoformat(),
        "openapi_version": openapi_schema.get("openapi", "3.0.0"),
    }


# Serve React static files in production
frontend_dist = Path(__file__).parent.parent / "frontend" / "dist"
if frontend_dist.exists():
    app.mount("/", StaticFiles(directory=str(frontend_dist), html=True), name="static")

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000, reload=True)
