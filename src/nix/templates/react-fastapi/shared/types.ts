// Shared TypeScript types for React FastAPI Template
// These types should match the Pydantic models in the backend

export interface User {
  id: number;
  username: string;
  email: string;
  is_active: boolean;
  created_at: string;
}

export interface UserCreate {
  username: string;
  email: string;
  password: string;
}

export interface Token {
  access_token: string;
  token_type: string;
}

export interface LoginCredentials {
  username: string;
  password: string;
}

export interface Task {
  id: number;
  title: string;
  description?: string;
  completed: boolean;
  created_at: string;
  updated_at: string;
  user_id: number;
}

export interface TaskCreate {
  title: string;
  description?: string;
}

export interface TaskUpdate {
  title?: string;
  description?: string;
  completed?: boolean;
}

export interface WebSocketMessage {
  type: string;
  data: Record<string, any>;
}

export interface ApiError {
  detail: string;
}

export interface HealthCheck {
  status: string;
  timestamp: string;
}

// WebSocket message types
export type WSMessageType = 
  | 'task_created'
  | 'task_updated'
  | 'task_deleted'
  | 'echo'
  | 'error';

export interface WSTaskCreated {
  type: 'task_created';
  data: {
    id: number;
    title: string;
    user: string;
  };
}

export interface WSTaskUpdated {
  type: 'task_updated';
  data: {
    id: number;
    title: string;
    completed: boolean;
    user: string;
  };
}

export interface WSTaskDeleted {
  type: 'task_deleted';
  data: {
    id: number;
    user: string;
  };
}

export interface WSEcho {
  type: 'echo';
  data: any;
}

export interface WSError {
  type: 'error';
  data: {
    message: string;
  };
}

export type WSMessage = WSTaskCreated | WSTaskUpdated | WSTaskDeleted | WSEcho | WSError;