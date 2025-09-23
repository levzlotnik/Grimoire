import type {
  DashboardStats,
  MonthlyData,
  DeviceData,
  HourlyActivity,
  Transaction,
  ApiDocsResponse
} from '../types/api'

export class ApiError extends Error {
  public status: number
  public detail: string

  constructor(status: number, detail: string) {
    super(detail)
    this.status = status
    this.detail = detail
    this.name = 'ApiError'
  }
}

export class ApiClient {
  private baseUrl: string
  private defaultHeaders: HeadersInit

  constructor(baseUrl: string = '/api') {
    this.baseUrl = baseUrl
    this.defaultHeaders = {
      'Content-Type': 'application/json',
    }
  }

  /**
   * Generic request method with type safety and error handling
   */
  private async request<T>(
    endpoint: string,
    options: RequestInit = {}
  ): Promise<T> {
    const url = `${this.baseUrl}${endpoint}`

    const config: RequestInit = {
      headers: {
        ...this.defaultHeaders,
        ...options.headers,
      },
      ...options,
    }

    try {
      const response = await fetch(url, config)

      if (!response.ok) {
        const errorData = await response.json().catch(() => ({}))
        throw new ApiError(
          response.status,
          errorData.detail || `HTTP ${response.status}: ${response.statusText}`
        )
      }

      const data = await response.json()
      return data as T
    } catch (error) {
      if (error instanceof ApiError) {
        throw error
      }

      // Network or other errors
      throw new ApiError(0, `Network error: ${error instanceof Error ? error.message : 'Unknown error'}`)
    }
  }

  /**
   * GET request helper
   */
  private async get<T>(endpoint: string): Promise<T> {
    return this.request<T>(endpoint, { method: 'GET' })
  }

  /**
   * POST request helper
   */
  private async post<T>(endpoint: string, data?: unknown): Promise<T> {
    return this.request<T>(endpoint, {
      method: 'POST',
      body: data ? JSON.stringify(data) : undefined,
    })
  }

  /**
   * PUT request helper
   */
  private async put<T>(endpoint: string, data?: unknown): Promise<T> {
    return this.request<T>(endpoint, {
      method: 'PUT',
      body: data ? JSON.stringify(data) : undefined,
    })
  }

  /**
   * DELETE request helper
   */
  private async delete<T>(endpoint: string): Promise<T> {
    return this.request<T>(endpoint, { method: 'DELETE' })
  }

  // Dashboard API methods
  async getDashboardStats(): Promise<DashboardStats> {
    return this.get<DashboardStats>('/dashboard/stats')
  }

  async getMonthlyData(): Promise<MonthlyData[]> {
    return this.get<MonthlyData[]>('/dashboard/monthly-data')
  }

  async getDeviceData(): Promise<DeviceData[]> {
    return this.get<DeviceData[]>('/dashboard/device-distribution')
  }

  async getHourlyActivity(): Promise<HourlyActivity[]> {
    return this.get<HourlyActivity[]>('/dashboard/hourly-activity')
  }

  async getTransactions(): Promise<Transaction[]> {
    return this.get<Transaction[]>('/dashboard/transactions')
  }

  // Example of other API methods you might add
  async createTransaction(transaction: Omit<Transaction, 'id'>): Promise<Transaction> {
    return this.post<Transaction>('/transactions', transaction)
  }

  async updateTransaction(id: string, updates: Partial<Transaction>): Promise<Transaction> {
    return this.put<Transaction>(`/transactions/${id}`, updates)
  }

  async deleteTransaction(id: string): Promise<void> {
    return this.delete<void>(`/transactions/${id}`)
  }

  // API Documentation methods
  async getApiEndpoints(): Promise<ApiDocsResponse> {
    return this.get<ApiDocsResponse>('/docs/endpoints')
  }

  /**
   * Set authorization header for authenticated requests
   */
  setAuthToken(token: string): void {
    this.defaultHeaders = {
      ...this.defaultHeaders,
      Authorization: `Bearer ${token}`,
    }
  }

  /**
   * Clear authorization header
   */
  clearAuthToken(): void {
    const { Authorization, ...headers } = this.defaultHeaders as any
    this.defaultHeaders = headers
  }
}

// Create and export a singleton instance
export const apiClient = new ApiClient()

// Export the class for testing or multiple instances
export default ApiClient
