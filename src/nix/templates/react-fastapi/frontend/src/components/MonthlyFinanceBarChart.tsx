import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts'
import type { MonthlyData } from '../types/api'

interface MonthlyFinanceBarChartProps {
  data: MonthlyData[]
  title?: string
}

export function MonthlyFinanceBarChart({ data, title = "Monthly Financial Overview" }: MonthlyFinanceBarChartProps) {
  return (
    <div className="bg-white p-6 rounded-lg shadow">
      <h3 className="text-lg font-medium text-gray-900 mb-4">{title}</h3>
      <ResponsiveContainer width="100%" height={300}>
        <BarChart data={data}>
          <CartesianGrid strokeDasharray="3 3" />
          <XAxis dataKey="name" />
          <YAxis />
          <Tooltip
            formatter={(value: number, name: string) => [
              `$${value.toLocaleString()}`,
              name === 'revenue' ? 'Revenue' : 'Expenses'
            ]}
          />
          <Legend />
          <Bar dataKey="revenue" fill="#10B981" name="Revenue" />
          <Bar dataKey="expenses" fill="#EF4444" name="Expenses" />
        </BarChart>
      </ResponsiveContainer>
    </div>
  )
}