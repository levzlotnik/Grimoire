export interface DashboardStats {
  total_users: number
  revenue: number
  orders: number
  conversion: number
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
  [key: string]: string | number
}

export interface HourlyActivity {
  time: string
  value: number
}

export interface Transaction {
  id: string
  user: string
  amount: string
  status: string
}