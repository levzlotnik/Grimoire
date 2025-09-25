import { PieChart, Pie, Cell, Tooltip, ResponsiveContainer } from 'recharts'
import type { DeviceData } from '../types/api'

interface DeviceDistributionPieChartProps {
  data: DeviceData[]
  title?: string
}

export function DeviceDistributionPieChart({ data, title = "Device Distribution" }: DeviceDistributionPieChartProps) {
  // Cast data to any to satisfy Recharts' type requirements
  const chartData = data as any[]
  
  return (
    <div className="bg-white p-6 rounded-lg shadow">
      <h3 className="text-lg font-medium text-gray-900 mb-4">{title}</h3>
      <ResponsiveContainer width="100%" height={300}>
        <PieChart>
          <Pie
            data={chartData}
            cx="50%"
            cy="50%"
            labelLine={false}
            label={({ name, percent }) => `${name} ${((percent as number) * 100).toFixed(0)}%`}
            outerRadius={80}
            fill="#8884d8"
            dataKey="value"
          >
            {chartData.map((entry, index) => (
              <Cell key={`cell-${index}`} fill={entry.color} />
            ))}
          </Pie>
          <Tooltip />
        </PieChart>
      </ResponsiveContainer>
    </div>
  )
}