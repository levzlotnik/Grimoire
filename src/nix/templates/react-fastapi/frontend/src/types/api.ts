// API Response Types
export interface DashboardStats {
  total_users: number
  total_revenue: number
  total_orders: number
  growth_rate: number
}

export interface MonthlyData {
  name: string
  revenue: number
  expenses: number
}

export interface DeviceData {
  name: string
  value: number
  color: string
}

export interface HourlyActivity {
  time: string
  activity: number
}

export interface Transaction {
  id: string
  user: string
  description: string
  amount: number
  status: 'completed' | 'pending' | 'failed'
  date: string
}

// API Documentation Types
export interface ApiEndpoint {
  method: string
  path: string
  summary: string
  description?: string
  tags: string[]
  security?: string[]
  parameters?: ApiParameter[]
  requestBody?: ApiRequestBody
  responses?: Record<string, ApiResponseSchema>
  examples?: {
    request?: string
    response?: string
  }
}

export interface ApiParameter {
  name: string
  in: 'path' | 'query' | 'header'
  required: boolean
  type: string
  description?: string
  example?: any
}

export interface ApiRequestBody {
  description?: string
  content: Record<string, {
    schema: any
    example?: any
  }>
}

export interface ApiResponseSchema {
  description: string
  content?: Record<string, {
    schema: any
    example?: any
  }>
}

export interface ApiDocsResponse {
  endpoints: ApiEndpoint[]
  tags: Record<string, string>
}

// Generic API Response wrapper
export interface ApiResponse<T> {
  data: T
  success: boolean
  message?: string
}