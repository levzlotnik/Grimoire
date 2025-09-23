import { useState, useEffect } from 'react'
import { apiClient, ApiError } from '../services/apiClient'
import { StatCard } from '../components/StatCard'
import { MonthlyFinanceBarChart } from '../components/MonthlyFinanceBarChart'
import { DeviceDistributionPieChart } from '../components/DeviceDistributionPieChart'
import { HourlyActivityLineChart } from '../components/HourlyActivityLineChart'
import { TransactionTable } from '../components/TransactionTable'
import type {
  DashboardStats,
  MonthlyData,
  DeviceData,
  HourlyActivity,
  Transaction
} from '../types/api'

export function Dashboard() {
  const [stats, setStats] = useState<DashboardStats | null>(null)
  const [monthlyData, setMonthlyData] = useState<MonthlyData[]>([])
  const [deviceData, setDeviceData] = useState<DeviceData[]>([])
  const [hourlyData, setHourlyData] = useState<HourlyActivity[]>([])
  const [transactions, setTransactions] = useState<Transaction[]>([])
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    const fetchDashboardData = async () => {
      try {
        setError(null)
        
        const [stats, monthlyData, deviceData, hourlyData, transactions] = await Promise.all([
          apiClient.getDashboardStats(),
          apiClient.getMonthlyData(),
          apiClient.getDeviceData(),
          apiClient.getHourlyActivity(),
          apiClient.getTransactions(),
        ])

        setStats(stats)
        setMonthlyData(monthlyData)
        setDeviceData(deviceData)
        setHourlyData(hourlyData)
        setTransactions(transactions)
      } catch (error) {
        const errorMessage = error instanceof ApiError 
          ? `API Error (${error.status}): ${error.detail}`
          : 'Failed to fetch dashboard data'
        
        console.error('Dashboard fetch error:', error)
        setError(errorMessage)
      } finally {
        setLoading(false)
      }
    }

    fetchDashboardData()
  }, [])

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-50 py-12 flex items-center justify-center">
        <div className="text-lg text-gray-600">Loading dashboard...</div>
      </div>
    )
  }

  if (error) {
    return (
      <div className="min-h-screen bg-gray-50 py-12 flex items-center justify-center">
        <div className="text-center">
          <div className="text-lg text-red-600 mb-4">Error Loading Dashboard</div>
          <div className="text-sm text-gray-600">{error}</div>
          <button 
            onClick={() => window.location.reload()} 
            className="mt-4 px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700"
          >
            Retry
          </button>
        </div>
      </div>
    )
  }

  return (
    <div className="min-h-screen bg-gray-50 py-12">
      <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">Dashboard</h1>
          <p className="mt-2 text-sm text-gray-600">
            Overview of key metrics and data visualizations
          </p>
        </div>

        {/* Stats Cards */}
        <div className="grid grid-cols-1 gap-5 sm:grid-cols-2 lg:grid-cols-4 mb-8">
          <StatCard
            title="Total Users"
            value={stats?.total_users?.toLocaleString() || '0'}
            icon={<span className="text-white text-sm font-medium">U</span>}
            iconColor="bg-indigo-500"
          />
          <StatCard
            title="Revenue"
            value={`$${stats?.total_revenue?.toLocaleString() || '0'}`}
            icon={<span className="text-white text-sm font-medium">R</span>}
            iconColor="bg-green-500"
          />
          <StatCard
            title="Orders"
            value={stats?.total_orders?.toLocaleString() || '0'}
            icon={<span className="text-white text-sm font-medium">O</span>}
            iconColor="bg-yellow-500"
          />
          <StatCard
            title="Growth Rate"
            value={`${stats?.growth_rate || '0'}%`}
            icon={<span className="text-white text-sm font-medium">C</span>}
            iconColor="bg-red-500"
          />
        </div>

        {/* Charts Grid */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-8 mb-8">
          <MonthlyFinanceBarChart data={monthlyData} />
          <DeviceDistributionPieChart data={deviceData} />
        </div>

        {/* Full-width charts */}
        <div className="mb-8">
          <HourlyActivityLineChart data={hourlyData} />
        </div>

        {/* Transactions Table */}
        <TransactionTable data={transactions} />
      </div>
    </div>
  )
}