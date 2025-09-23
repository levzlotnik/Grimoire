import { Edit, Trash2, ChevronLeft, ChevronRight } from 'lucide-react'
import { clsx } from 'clsx'

export interface TableData {
  id: number
  name: string
  email: string
  role: string
  status: 'active' | 'inactive' | 'pending'
  actions: string
}

interface ComponentSectionProps {
  title: string
  children: React.ReactNode
  codeExample?: string
}

interface TablesShowcaseProps {
  ComponentSection: React.ComponentType<ComponentSectionProps>
  tableData: TableData[]
  sortField: keyof TableData
  setSortField: (field: keyof TableData) => void
  sortDirection: 'asc' | 'desc'
  setSortDirection: (direction: 'asc' | 'desc') => void
  currentPage: number
  setCurrentPage: (page: number) => void
}

export function TablesShowcase({
  ComponentSection,
  tableData,
  sortField,
  setSortField,
  sortDirection,
  setSortDirection,
  currentPage,
  setCurrentPage
}: TablesShowcaseProps) {
  const handleSort = (field: keyof TableData) => {
    if (field === sortField) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc')
    } else {
      setSortField(field)
      setSortDirection('asc')
    }
  }

  const sortedData = [...tableData].sort((a, b) => {
    const aVal = a[sortField]
    const bVal = b[sortField]
    if (aVal < bVal) return sortDirection === 'asc' ? -1 : 1
    if (aVal > bVal) return sortDirection === 'asc' ? 1 : -1
    return 0
  })

  return (
    <div className="space-y-8">
      <ComponentSection 
        title="Data Table with Sorting"
        codeExample={`<table className="min-w-full divide-y divide-gray-200">
  <thead className="bg-gray-50">
    <tr>
      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider cursor-pointer hover:bg-gray-100">
        Name
      </th>
    </tr>
  </thead>
  <tbody className="bg-white divide-y divide-gray-200">
    <tr className="hover:bg-gray-50">
      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
        John Doe
      </td>
    </tr>
  </tbody>
</table>`}
      >
        <div className="overflow-x-auto">
          <table className="min-w-full divide-y divide-gray-200">
            <thead className="bg-gray-50">
              <tr>
                {(['name', 'email', 'role', 'status'] as const).map((field) => (
                  <th
                    key={field}
                    onClick={() => handleSort(field)}
                    className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider cursor-pointer hover:bg-gray-100"
                  >
                    <div className="flex items-center space-x-1">
                      <span className="capitalize">{field}</span>
                      {sortField === field && (
                        <span className="text-blue-600">
                          {sortDirection === 'asc' ? '↑' : '↓'}
                        </span>
                      )}
                    </div>
                  </th>
                ))}
                <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                  Actions
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {sortedData.map((row) => (
                <tr key={row.id} className="hover:bg-gray-50">
                  <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                    {row.name}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                    {row.email}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                    {row.role}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap">
                    <span
                      className={clsx(
                        'inline-flex px-2 py-1 text-xs font-semibold rounded-full',
                        {
                          'bg-green-100 text-green-800': row.status === 'active',
                          'bg-red-100 text-red-800': row.status === 'inactive',
                          'bg-yellow-100 text-yellow-800': row.status === 'pending'
                        }
                      )}
                    >
                      {row.status}
                    </span>
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                    <div className="flex items-center space-x-2">
                      <button className="text-blue-600 hover:text-blue-700">
                        <Edit className="h-4 w-4" />
                      </button>
                      <button className="text-red-600 hover:text-red-700">
                        <Trash2 className="h-4 w-4" />
                      </button>
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
        
        <div className="mt-4 flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <button
              onClick={() => setCurrentPage(prev => Math.max(1, prev - 1))}
              disabled={currentPage === 1}
              className="px-3 py-1 border border-gray-300 rounded-md text-sm disabled:opacity-50 disabled:cursor-not-allowed hover:bg-gray-50"
            >
              <ChevronLeft className="h-4 w-4" />
            </button>
            <span className="text-sm text-gray-700">
              Page {currentPage} of 2
            </span>
            <button
              onClick={() => setCurrentPage(prev => Math.min(2, prev + 1))}
              disabled={currentPage === 2}
              className="px-3 py-1 border border-gray-300 rounded-md text-sm disabled:opacity-50 disabled:cursor-not-allowed hover:bg-gray-50"
            >
              <ChevronRight className="h-4 w-4" />
            </button>
          </div>
          <span className="text-sm text-gray-700">
            Showing 4 results
          </span>
        </div>
      </ComponentSection>
    </div>
  )
}