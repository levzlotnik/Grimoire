import { useState } from 'react'
import { clsx } from 'clsx'
import { CodeBlock } from '../components/CodeBlock'
import {
  X,
  AlertCircle,
  Info,
  CheckCircle,
  XCircle,
  Heart,
  Star,
  Bell,
  Settings,
  Plus,
  Download,
  Edit,
  Trash2,
  ChevronDown,
  Upload,
  Filter,
  MoreVertical,
  ChevronLeft,
  ChevronRight,
  Menu,
  MessageSquare,
  Loader,
  Eye,
  EyeOff
} from 'lucide-react'

type TabId = 'buttons' | 'forms' | 'alerts' | 'tables' | 'cards' | 'modals' | 'navigation' | 'feedback'

interface TableData {
  id: number
  name: string
  email: string
  role: string
  status: 'active' | 'inactive' | 'pending'
  actions: string
}

export function Showcase() {
  const [activeTab, setActiveTab] = useState<TabId>('buttons')
  const [showCode, setShowCode] = useState<Record<string, boolean>>({})
  
  // Form states
  const [formData, setFormData] = useState({
    name: '',
    email: '',
    password: '',
    category: '',
    newsletter: false,
    plan: 'basic',
    message: ''
  })
  const [showPassword, setShowPassword] = useState(false)
  
  // Alert states
  const [alerts, setAlerts] = useState({
    success: true,
    error: true,
    warning: true,
    info: true
  })
  
  // Modal states
  const [modals, setModals] = useState({
    basic: false,
    confirmation: false,
    form: false
  })
  
  // Table states
  const [tableData] = useState<TableData[]>([
    { id: 1, name: 'John Doe', email: 'john@example.com', role: 'Admin', status: 'active', actions: '' },
    { id: 2, name: 'Jane Smith', email: 'jane@example.com', role: 'User', status: 'active', actions: '' },
    { id: 3, name: 'Bob Johnson', email: 'bob@example.com', role: 'User', status: 'inactive', actions: '' },
    { id: 4, name: 'Alice Brown', email: 'alice@example.com', role: 'Moderator', status: 'pending', actions: '' }
  ])
  const [sortField, setSortField] = useState<keyof TableData>('name')
  const [sortDirection, setSortDirection] = useState<'asc' | 'desc'>('asc')
  const [currentPage, setCurrentPage] = useState(1)
  
  // Navigation states
  const [currentBreadcrumb, setCurrentBreadcrumb] = useState(['Home', 'Components', 'Showcase'])
  const [navTab, setNavTab] = useState('tab1')
  
  // Feedback states
  const [progress, setProgress] = useState(65)
  const [isLoading, setIsLoading] = useState(false)

  const tabs: { id: TabId; label: string; icon: React.ReactNode }[] = [
    { id: 'buttons', label: 'Buttons', icon: <Plus className="h-4 w-4" /> },
    { id: 'forms', label: 'Forms', icon: <Edit className="h-4 w-4" /> },
    { id: 'alerts', label: 'Alerts', icon: <AlertCircle className="h-4 w-4" /> },
    { id: 'tables', label: 'Tables', icon: <Filter className="h-4 w-4" /> },
    { id: 'cards', label: 'Cards', icon: <Star className="h-4 w-4" /> },
    { id: 'modals', label: 'Modals', icon: <MessageSquare className="h-4 w-4" /> },
    { id: 'navigation', label: 'Navigation', icon: <Menu className="h-4 w-4" /> },
    { id: 'feedback', label: 'Feedback', icon: <Loader className="h-4 w-4" /> }
  ]

  const toggleCode = (section: string) => {
    setShowCode(prev => ({ ...prev, [section]: !prev[section] }))
  }

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

  const simulateProgress = () => {
    setIsLoading(true)
    setProgress(0)
    const interval = setInterval(() => {
      setProgress(prev => {
        if (prev >= 100) {
          clearInterval(interval)
          setIsLoading(false)
          return 100
        }
        return prev + 10
      })
    }, 200)
  }

  const ComponentSection = ({ title, children, codeExample }: { 
    title: string
    children: React.ReactNode
    codeExample?: string 
  }) => (
    <div className="mb-8">
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-lg font-semibold text-gray-900">{title}</h3>
        {codeExample && (
          <button
            onClick={() => toggleCode(title)}
            className="text-sm text-blue-600 hover:text-blue-700"
          >
            {showCode[title] ? 'Hide Code' : 'Show Code'}
          </button>
        )}
      </div>
      <div className="bg-white rounded-lg border border-gray-200 p-6 mb-4">
        {children}
      </div>
      {codeExample && showCode[title] && (
        <CodeBlock code={codeExample} language="tsx" title={`${title} Example`} />
      )}
    </div>
  )

  const renderButtonsTab = () => (
    <div className="space-y-8">
      <ComponentSection 
        title="Primary Buttons"
        codeExample={`<button className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors">
  Primary Button
</button>
<button className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors disabled:bg-gray-300 disabled:cursor-not-allowed" disabled>
  Disabled
</button>`}
      >
        <div className="flex flex-wrap gap-3">
          <button className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors">
            Primary Button
          </button>
          <button className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors disabled:bg-gray-300 disabled:cursor-not-allowed" disabled>
            Disabled
          </button>
          <button className="px-6 py-3 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors text-lg">
            Large Button
          </button>
          <button className="px-3 py-1 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors text-sm">
            Small Button
          </button>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Secondary Buttons"
        codeExample={`<button className="px-4 py-2 border border-gray-300 text-gray-700 rounded-md hover:bg-gray-50 transition-colors">
  Secondary Button
</button>`}
      >
        <div className="flex flex-wrap gap-3">
          <button className="px-4 py-2 border border-gray-300 text-gray-700 rounded-md hover:bg-gray-50 transition-colors">
            Secondary Button
          </button>
          <button className="px-4 py-2 border border-red-300 text-red-700 rounded-md hover:bg-red-50 transition-colors">
            Danger Button
          </button>
          <button className="px-4 py-2 border border-green-300 text-green-700 rounded-md hover:bg-green-50 transition-colors">
            Success Button
          </button>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Icon Buttons"
        codeExample={`<button className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-100 rounded-md transition-colors">
  <Heart className="h-5 w-5" />
</button>`}
      >
        <div className="flex flex-wrap gap-3">
          <button className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-100 rounded-md transition-colors">
            <Heart className="h-5 w-5" />
          </button>
          <button className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-100 rounded-md transition-colors">
            <Star className="h-5 w-5" />
          </button>
          <button className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-100 rounded-md transition-colors">
            <Bell className="h-5 w-5" />
          </button>
          <button className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-100 rounded-md transition-colors">
            <Settings className="h-5 w-5" />
          </button>
          <button className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors">
            <Download className="h-4 w-4" />
            Download
          </button>
          <button className="flex items-center gap-2 px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700 transition-colors">
            <Upload className="h-4 w-4" />
            Upload
          </button>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Button Groups"
        codeExample={`<div className="inline-flex rounded-md shadow-sm">
  <button className="px-4 py-2 bg-blue-600 text-white rounded-l-md hover:bg-blue-700 transition-colors">
    Left
  </button>
  <button className="px-4 py-2 bg-blue-600 text-white border-l border-blue-500 hover:bg-blue-700 transition-colors">
    Center
  </button>
  <button className="px-4 py-2 bg-blue-600 text-white border-l border-blue-500 rounded-r-md hover:bg-blue-700 transition-colors">
    Right
  </button>
</div>`}
      >
        <div className="space-y-4">
          <div className="inline-flex rounded-md shadow-sm">
            <button className="px-4 py-2 bg-blue-600 text-white rounded-l-md hover:bg-blue-700 transition-colors">
              Left
            </button>
            <button className="px-4 py-2 bg-blue-600 text-white border-l border-blue-500 hover:bg-blue-700 transition-colors">
              Center
            </button>
            <button className="px-4 py-2 bg-blue-600 text-white border-l border-blue-500 rounded-r-md hover:bg-blue-700 transition-colors">
              Right
            </button>
          </div>
          
          <div className="inline-flex rounded-md shadow-sm">
            <button className="p-2 border border-gray-300 rounded-l-md hover:bg-gray-50 transition-colors">
              <Edit className="h-4 w-4" />
            </button>
            <button className="p-2 border-t border-b border-gray-300 hover:bg-gray-50 transition-colors">
              <Trash2 className="h-4 w-4" />
            </button>
            <button className="p-2 border border-gray-300 rounded-r-md hover:bg-gray-50 transition-colors">
              <MoreVertical className="h-4 w-4" />
            </button>
          </div>
        </div>
      </ComponentSection>
    </div>
  )

  const renderFormsTab = () => (
    <div className="space-y-8">
      <ComponentSection 
        title="Text Inputs"
        codeExample={`<input
  type="text"
  placeholder="Enter your name"
  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
/>`}
      >
        <div className="space-y-4 max-w-md">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">Name</label>
            <input
              type="text"
              placeholder="Enter your name"
              value={formData.name}
              onChange={(e) => setFormData(prev => ({ ...prev, name: e.target.value }))}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">Email</label>
            <input
              type="email"
              placeholder="Enter your email"
              value={formData.email}
              onChange={(e) => setFormData(prev => ({ ...prev, email: e.target.value }))}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">Password</label>
            <div className="relative">
              <input
                type={showPassword ? "text" : "password"}
                placeholder="Enter your password"
                value={formData.password}
                onChange={(e) => setFormData(prev => ({ ...prev, password: e.target.value }))}
                className="w-full px-3 py-2 pr-10 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              />
              <button
                type="button"
                onClick={() => setShowPassword(!showPassword)}
                className="absolute inset-y-0 right-0 pr-3 flex items-center text-gray-400 hover:text-gray-600"
              >
                {showPassword ? <EyeOff className="h-4 w-4" /> : <Eye className="h-4 w-4" />}
              </button>
            </div>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">Disabled Input</label>
            <input
              type="text"
              placeholder="This input is disabled"
              disabled
              className="w-full px-3 py-2 border border-gray-300 rounded-md bg-gray-100 text-gray-500 cursor-not-allowed"
            />
          </div>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Select Dropdowns"
        codeExample={`<select className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent">
  <option value="">Select an option</option>
  <option value="option1">Option 1</option>
  <option value="option2">Option 2</option>
</select>`}
      >
        <div className="space-y-4 max-w-md">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">Category</label>
            <div className="relative">
              <select
                value={formData.category}
                onChange={(e) => setFormData(prev => ({ ...prev, category: e.target.value }))}
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent appearance-none"
              >
                <option value="">Select a category</option>
                <option value="technology">Technology</option>
                <option value="design">Design</option>
                <option value="business">Business</option>
                <option value="marketing">Marketing</option>
              </select>
              <ChevronDown className="absolute right-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400 pointer-events-none" />
            </div>
          </div>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Checkboxes and Radio Buttons"
        codeExample={`<label className="flex items-center">
  <input
    type="checkbox"
    className="rounded border-gray-300 text-blue-600 focus:ring-blue-500"
  />
  <span className="ml-2 text-sm text-gray-700">Subscribe to newsletter</span>
</label>`}
      >
        <div className="space-y-6">
          <div>
            <h4 className="text-sm font-medium text-gray-700 mb-3">Checkbox</h4>
            <label className="flex items-center">
              <input
                type="checkbox"
                checked={formData.newsletter}
                onChange={(e) => setFormData(prev => ({ ...prev, newsletter: e.target.checked }))}
                className="rounded border-gray-300 text-blue-600 focus:ring-blue-500"
              />
              <span className="ml-2 text-sm text-gray-700">Subscribe to newsletter</span>
            </label>
          </div>
          
          <div>
            <h4 className="text-sm font-medium text-gray-700 mb-3">Radio Buttons</h4>
            <div className="space-y-2">
              {['basic', 'premium', 'enterprise'].map((plan) => (
                <label key={plan} className="flex items-center">
                  <input
                    type="radio"
                    name="plan"
                    value={plan}
                    checked={formData.plan === plan}
                    onChange={(e) => setFormData(prev => ({ ...prev, plan: e.target.value }))}
                    className="border-gray-300 text-blue-600 focus:ring-blue-500"
                  />
                  <span className="ml-2 text-sm text-gray-700 capitalize">{plan} Plan</span>
                </label>
              ))}
            </div>
          </div>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Textarea"
        codeExample={`<textarea
  placeholder="Enter your message"
  rows={4}
  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent resize-vertical"
/>`}
      >
        <div className="max-w-md">
          <label className="block text-sm font-medium text-gray-700 mb-2">Message</label>
          <textarea
            placeholder="Enter your message"
            rows={4}
            value={formData.message}
            onChange={(e) => setFormData(prev => ({ ...prev, message: e.target.value }))}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent resize-vertical"
          />
        </div>
      </ComponentSection>
    </div>
  )

  const renderAlertsTab = () => (
    <div className="space-y-8">
      <ComponentSection 
        title="Alert Types"
        codeExample={`<div className="bg-green-50 border border-green-200 rounded-md p-4">
  <div className="flex">
    <CheckCircle className="h-5 w-5 text-green-400" />
    <div className="ml-3">
      <h3 className="text-sm font-medium text-green-800">Success</h3>
      <p className="mt-1 text-sm text-green-700">Your changes have been saved successfully.</p>
    </div>
  </div>
</div>`}
      >
        <div className="space-y-4">
          {alerts.success && (
            <div className="bg-green-50 border border-green-200 rounded-md p-4">
              <div className="flex">
                <CheckCircle className="h-5 w-5 text-green-400" />
                <div className="ml-3 flex-1">
                  <h3 className="text-sm font-medium text-green-800">Success</h3>
                  <p className="mt-1 text-sm text-green-700">Your changes have been saved successfully.</p>
                </div>
                <button
                  onClick={() => setAlerts(prev => ({ ...prev, success: false }))}
                  className="ml-3 text-green-400 hover:text-green-600"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
            </div>
          )}

          {alerts.error && (
            <div className="bg-red-50 border border-red-200 rounded-md p-4">
              <div className="flex">
                <XCircle className="h-5 w-5 text-red-400" />
                <div className="ml-3 flex-1">
                  <h3 className="text-sm font-medium text-red-800">Error</h3>
                  <p className="mt-1 text-sm text-red-700">There was an error processing your request.</p>
                </div>
                <button
                  onClick={() => setAlerts(prev => ({ ...prev, error: false }))}
                  className="ml-3 text-red-400 hover:text-red-600"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
            </div>
          )}

          {alerts.warning && (
            <div className="bg-yellow-50 border border-yellow-200 rounded-md p-4">
              <div className="flex">
                <AlertCircle className="h-5 w-5 text-yellow-400" />
                <div className="ml-3 flex-1">
                  <h3 className="text-sm font-medium text-yellow-800">Warning</h3>
                  <p className="mt-1 text-sm text-yellow-700">Please review your settings before continuing.</p>
                </div>
                <button
                  onClick={() => setAlerts(prev => ({ ...prev, warning: false }))}
                  className="ml-3 text-yellow-400 hover:text-yellow-600"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
            </div>
          )}

          {alerts.info && (
            <div className="bg-blue-50 border border-blue-200 rounded-md p-4">
              <div className="flex">
                <Info className="h-5 w-5 text-blue-400" />
                <div className="ml-3 flex-1">
                  <h3 className="text-sm font-medium text-blue-800">Information</h3>
                  <p className="mt-1 text-sm text-blue-700">New features are now available in your dashboard.</p>
                </div>
                <button
                  onClick={() => setAlerts(prev => ({ ...prev, info: false }))}
                  className="ml-3 text-blue-400 hover:text-blue-600"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
            </div>
          )}

          <button
            onClick={() => setAlerts({ success: true, error: true, warning: true, info: true })}
            className="px-4 py-2 bg-gray-600 text-white rounded-md hover:bg-gray-700 transition-colors"
          >
            Reset All Alerts
          </button>
        </div>
      </ComponentSection>
    </div>
  )

  const renderTablesTab = () => (
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

  const renderCardsTab = () => (
    <div className="space-y-8">
      <ComponentSection 
        title="Basic Cards"
        codeExample={`<div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6">
  <h3 className="text-lg font-semibold text-gray-900 mb-2">Card Title</h3>
  <p className="text-gray-600 mb-4">This is a basic card with some content.</p>
  <button className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors">
    Action
  </button>
</div>`}
      >
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          <div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6">
            <h3 className="text-lg font-semibold text-gray-900 mb-2">Basic Card</h3>
            <p className="text-gray-600 mb-4">This is a basic card with some content and an action button.</p>
            <button className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors">
              Learn More
            </button>
          </div>

          <div className="bg-white rounded-lg border border-gray-200 shadow-sm overflow-hidden">
            <div className="h-32 bg-gradient-to-r from-blue-500 to-purple-600"></div>
            <div className="p-6">
              <h3 className="text-lg font-semibold text-gray-900 mb-2">Card with Image</h3>
              <p className="text-gray-600 mb-4">This card includes a header image area.</p>
              <button className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors">
                View Details
              </button>
            </div>
          </div>

          <div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6">
            <div className="flex items-center justify-between mb-4">
              <h3 className="text-lg font-semibold text-gray-900">Card with Actions</h3>
              <div className="flex items-center space-x-2">
                <button className="p-1 text-gray-400 hover:text-gray-600">
                  <Heart className="h-4 w-4" />
                </button>
                <button className="p-1 text-gray-400 hover:text-gray-600">
                  <Star className="h-4 w-4" />
                </button>
              </div>
            </div>
            <p className="text-gray-600 mb-4">This card has icon actions in the header.</p>
            <div className="flex space-x-2">
              <button className="flex-1 px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors">
                Primary
              </button>
              <button className="flex-1 px-4 py-2 border border-gray-300 text-gray-700 rounded-md hover:bg-gray-50 transition-colors">
                Secondary
              </button>
            </div>
          </div>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Pricing Cards"
        codeExample={`<div className="bg-white rounded-lg border-2 border-blue-200 shadow-sm p-6 relative">
  <div className="absolute top-0 left-1/2 transform -translate-x-1/2 -translate-y-1/2">
    <span className="bg-blue-600 text-white px-3 py-1 rounded-full text-xs font-medium">
      Popular
    </span>
  </div>
  <div className="text-center">
    <h3 className="text-lg font-semibold text-gray-900">Pro</h3>
    <div className="mt-4 mb-6">
      <span className="text-4xl font-bold text-gray-900">$29</span>
      <span className="text-gray-600">/month</span>
    </div>
  </div>
</div>`}
      >
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
          <div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6">
            <div className="text-center">
              <h3 className="text-lg font-semibold text-gray-900">Basic</h3>
              <div className="mt-4 mb-6">
                <span className="text-4xl font-bold text-gray-900">$9</span>
                <span className="text-gray-600">/month</span>
              </div>
            </div>
            <ul className="space-y-3 mb-6">
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">5 Projects</span>
              </li>
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">10GB Storage</span>
              </li>
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">Email Support</span>
              </li>
            </ul>
            <button className="w-full px-4 py-2 border border-gray-300 text-gray-700 rounded-md hover:bg-gray-50 transition-colors">
              Choose Plan
            </button>
          </div>

          <div className="bg-white rounded-lg border-2 border-blue-200 shadow-sm p-6 relative">
            <div className="absolute top-0 left-1/2 transform -translate-x-1/2 -translate-y-1/2">
              <span className="bg-blue-600 text-white px-3 py-1 rounded-full text-xs font-medium">
                Popular
              </span>
            </div>
            <div className="text-center">
              <h3 className="text-lg font-semibold text-gray-900">Pro</h3>
              <div className="mt-4 mb-6">
                <span className="text-4xl font-bold text-gray-900">$29</span>
                <span className="text-gray-600">/month</span>
              </div>
            </div>
            <ul className="space-y-3 mb-6">
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">25 Projects</span>
              </li>
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">100GB Storage</span>
              </li>
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">Priority Support</span>
              </li>
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">Advanced Analytics</span>
              </li>
            </ul>
            <button className="w-full px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors">
              Choose Plan
            </button>
          </div>

          <div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6">
            <div className="text-center">
              <h3 className="text-lg font-semibold text-gray-900">Enterprise</h3>
              <div className="mt-4 mb-6">
                <span className="text-4xl font-bold text-gray-900">$99</span>
                <span className="text-gray-600">/month</span>
              </div>
            </div>
            <ul className="space-y-3 mb-6">
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">Unlimited Projects</span>
              </li>
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">1TB Storage</span>
              </li>
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">24/7 Support</span>
              </li>
              <li className="flex items-center">
                <CheckCircle className="h-5 w-5 text-green-500 mr-2" />
                <span className="text-gray-600">Custom Integrations</span>
              </li>
            </ul>
            <button className="w-full px-4 py-2 border border-gray-300 text-gray-700 rounded-md hover:bg-gray-50 transition-colors">
              Contact Sales
            </button>
          </div>
        </div>
      </ComponentSection>
    </div>
  )

  const renderModalsTab = () => (
    <div className="space-y-8">
      <ComponentSection 
        title="Modal Examples"
        codeExample={`// Modal component with backdrop
{isOpen && (
  <div className="fixed inset-0 bg-gray-600 bg-opacity-50 overflow-y-auto h-full w-full z-50">
    <div className="relative top-20 mx-auto p-5 border w-96 shadow-lg rounded-md bg-white">
      <div className="mt-3 text-center">
        <h3 className="text-lg font-medium text-gray-900">Modal Title</h3>
        <div className="mt-2 px-7 py-3">
          <p className="text-sm text-gray-500">Modal content goes here</p>
        </div>
      </div>
    </div>
  </div>
)}`}
      >
        <div className="space-y-4">
          <div className="flex flex-wrap gap-4">
            <button
              onClick={() => setModals(prev => ({ ...prev, basic: true }))}
              className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors"
            >
              Open Basic Modal
            </button>
            <button
              onClick={() => setModals(prev => ({ ...prev, confirmation: true }))}
              className="px-4 py-2 bg-red-600 text-white rounded-md hover:bg-red-700 transition-colors"
            >
              Open Confirmation Modal
            </button>
            <button
              onClick={() => setModals(prev => ({ ...prev, form: true }))}
              className="px-4 py-2 bg-green-600 text-white rounded-md hover:bg-green-700 transition-colors"
            >
              Open Form Modal
            </button>
          </div>
        </div>
      </ComponentSection>

      {/* Basic Modal */}
      {modals.basic && (
        <div className="fixed inset-0 bg-gray-600 bg-opacity-50 overflow-y-auto h-full w-full z-50">
          <div className="relative top-20 mx-auto p-5 border w-96 shadow-lg rounded-md bg-white">
            <div className="flex justify-between items-center mb-4">
              <h3 className="text-lg font-medium text-gray-900">Basic Modal</h3>
              <button
                onClick={() => setModals(prev => ({ ...prev, basic: false }))}
                className="text-gray-400 hover:text-gray-600"
              >
                <X className="h-5 w-5" />
              </button>
            </div>
            <div className="mt-2 mb-6">
              <p className="text-sm text-gray-500">
                This is a basic modal dialog. It can contain any content you want to display to the user.
              </p>
            </div>
            <div className="flex justify-end space-x-3">
              <button
                onClick={() => setModals(prev => ({ ...prev, basic: false }))}
                className="px-4 py-2 border border-gray-300 text-gray-700 rounded-md hover:bg-gray-50 transition-colors"
              >
                Cancel
              </button>
              <button
                onClick={() => setModals(prev => ({ ...prev, basic: false }))}
                className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors"
              >
                OK
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Confirmation Modal */}
      {modals.confirmation && (
        <div className="fixed inset-0 bg-gray-600 bg-opacity-50 overflow-y-auto h-full w-full z-50">
          <div className="relative top-20 mx-auto p-5 border w-96 shadow-lg rounded-md bg-white">
            <div className="flex items-center mb-4">
              <div className="flex-shrink-0">
                <AlertCircle className="h-6 w-6 text-red-400" />
              </div>
              <div className="ml-3">
                <h3 className="text-lg font-medium text-gray-900">Confirm Action</h3>
              </div>
            </div>
            <div className="mt-2 mb-6">
              <p className="text-sm text-gray-500">
                Are you sure you want to delete this item? This action cannot be undone.
              </p>
            </div>
            <div className="flex justify-end space-x-3">
              <button
                onClick={() => setModals(prev => ({ ...prev, confirmation: false }))}
                className="px-4 py-2 border border-gray-300 text-gray-700 rounded-md hover:bg-gray-50 transition-colors"
              >
                Cancel
              </button>
              <button
                onClick={() => setModals(prev => ({ ...prev, confirmation: false }))}
                className="px-4 py-2 bg-red-600 text-white rounded-md hover:bg-red-700 transition-colors"
              >
                Delete
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Form Modal */}
      {modals.form && (
        <div className="fixed inset-0 bg-gray-600 bg-opacity-50 overflow-y-auto h-full w-full z-50">
          <div className="relative top-20 mx-auto p-5 border w-96 shadow-lg rounded-md bg-white">
            <div className="flex justify-between items-center mb-4">
              <h3 className="text-lg font-medium text-gray-900">Add New Item</h3>
              <button
                onClick={() => setModals(prev => ({ ...prev, form: false }))}
                className="text-gray-400 hover:text-gray-600"
              >
                <X className="h-5 w-5" />
              </button>
            </div>
            <form className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">Name</label>
                <input
                  type="text"
                  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                  placeholder="Enter name"
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">Email</label>
                <input
                  type="email"
                  className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                  placeholder="Enter email"
                />
              </div>
              <div className="flex justify-end space-x-3 pt-4">
                <button
                  type="button"
                  onClick={() => setModals(prev => ({ ...prev, form: false }))}
                  className="px-4 py-2 border border-gray-300 text-gray-700 rounded-md hover:bg-gray-50 transition-colors"
                >
                  Cancel
                </button>
                <button
                  type="submit"
                  onClick={(e) => {
                    e.preventDefault()
                    setModals(prev => ({ ...prev, form: false }))
                  }}
                  className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors"
                >
                  Save
                </button>
              </div>
            </form>
          </div>
        </div>
      )}
    </div>
  )

  const renderNavigationTab = () => (
    <div className="space-y-8">
      <ComponentSection 
        title="Breadcrumbs"
        codeExample={`<nav className="flex" aria-label="Breadcrumb">
  <ol className="flex items-center space-x-4">
    <li>
      <a href="#" className="text-gray-400 hover:text-gray-500">Home</a>
    </li>
    <li>
      <ChevronRight className="h-4 w-4 text-gray-300" />
    </li>
    <li>
      <a href="#" className="text-gray-400 hover:text-gray-500">Components</a>
    </li>
  </ol>
</nav>`}
      >
        <nav className="flex" aria-label="Breadcrumb">
          <ol className="flex items-center space-x-4">
            {currentBreadcrumb.map((crumb, index) => (
              <li key={index} className="flex items-center">
                {index > 0 && <ChevronRight className="h-4 w-4 text-gray-300 mr-4" />}
                <button
                  onClick={() => setCurrentBreadcrumb(currentBreadcrumb.slice(0, index + 1))}
                  className={clsx(
                    index === currentBreadcrumb.length - 1
                      ? 'text-gray-900 font-medium'
                      : 'text-gray-400 hover:text-gray-500'
                  )}
                >
                  {crumb}
                </button>
              </li>
            ))}
          </ol>
        </nav>
      </ComponentSection>

      <ComponentSection 
        title="Tab Navigation"
        codeExample={`<div className="border-b border-gray-200">
  <nav className="-mb-px flex space-x-8">
    <button className="border-b-2 border-blue-500 text-blue-600 py-2 px-1 text-sm font-medium">
      Tab 1
    </button>
    <button className="border-b-2 border-transparent text-gray-500 hover:text-gray-700 py-2 px-1 text-sm font-medium">
      Tab 2
    </button>
  </nav>
</div>`}
      >
        <div>
          <div className="border-b border-gray-200">
            <nav className="-mb-px flex space-x-8">
              {['tab1', 'tab2', 'tab3'].map((tab) => (
                <button
                  key={tab}
                  onClick={() => setNavTab(tab)}
                  className={clsx(
                    'border-b-2 py-2 px-1 text-sm font-medium',
                    navTab === tab
                      ? 'border-blue-500 text-blue-600'
                      : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
                  )}
                >
                  Tab {tab.slice(-1)}
                </button>
              ))}
            </nav>
          </div>
          <div className="mt-4 p-4 bg-gray-50 rounded-md">
            <p className="text-sm text-gray-600">
              Content for {navTab} is displayed here.
            </p>
          </div>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Pagination"
        codeExample={`<div className="flex items-center justify-between">
  <div className="flex items-center space-x-2">
    <button className="px-3 py-2 border border-gray-300 rounded-md text-sm hover:bg-gray-50">
      Previous
    </button>
    <button className="px-3 py-2 bg-blue-600 text-white rounded-md text-sm">1</button>
    <button className="px-3 py-2 border border-gray-300 rounded-md text-sm hover:bg-gray-50">2</button>
  </div>
</div>`}
      >
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <button
              onClick={() => setCurrentPage(prev => Math.max(1, prev - 1))}
              disabled={currentPage === 1}
              className="px-3 py-2 border border-gray-300 rounded-md text-sm disabled:opacity-50 disabled:cursor-not-allowed hover:bg-gray-50"
            >
              Previous
            </button>
            {[1, 2, 3].map((page) => (
              <button
                key={page}
                onClick={() => setCurrentPage(page)}
                className={clsx(
                  'px-3 py-2 rounded-md text-sm',
                  currentPage === page
                    ? 'bg-blue-600 text-white'
                    : 'border border-gray-300 hover:bg-gray-50'
                )}
              >
                {page}
              </button>
            ))}
            <button
              onClick={() => setCurrentPage(prev => Math.min(3, prev + 1))}
              disabled={currentPage === 3}
              className="px-3 py-2 border border-gray-300 rounded-md text-sm disabled:opacity-50 disabled:cursor-not-allowed hover:bg-gray-50"
            >
              Next
            </button>
          </div>
          <span className="text-sm text-gray-700">
            Page {currentPage} of 3
          </span>
        </div>
      </ComponentSection>
    </div>
  )

  const renderFeedbackTab = () => (
    <div className="space-y-8">
      <ComponentSection 
        title="Loading Spinners"
        codeExample={`<div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>`}
      >
        <div className="flex items-center space-x-6">
          <div className="text-center">
            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto mb-2"></div>
            <span className="text-sm text-gray-600">Default</span>
          </div>
          <div className="text-center">
            <div className="animate-spin rounded-full h-6 w-6 border-2 border-gray-300 border-t-2 border-t-blue-600 mx-auto mb-2"></div>
            <span className="text-sm text-gray-600">Small</span>
          </div>
          <div className="text-center">
            <div className="animate-spin rounded-full h-12 w-12 border-4 border-gray-300 border-t-4 border-t-green-600 mx-auto mb-2"></div>
            <span className="text-sm text-gray-600">Large</span>
          </div>
          <div className="text-center">
            <Loader className="animate-spin h-8 w-8 text-blue-600 mx-auto mb-2" />
            <span className="text-sm text-gray-600">Icon</span>
          </div>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Progress Bars"
        codeExample={`<div className="w-full bg-gray-200 rounded-full h-2">
  <div className="bg-blue-600 h-2 rounded-full transition-all duration-300" style={{ width: '65%' }}></div>
</div>`}
      >
        <div className="space-y-6">
          <div>
            <div className="flex justify-between text-sm text-gray-600 mb-2">
              <span>Progress</span>
              <span>{progress}%</span>
            </div>
            <div className="w-full bg-gray-200 rounded-full h-2">
              <div 
                className="bg-blue-600 h-2 rounded-full transition-all duration-300" 
                style={{ width: `${progress}%` }}
              ></div>
            </div>
          </div>
          
          <div>
            <div className="flex justify-between text-sm text-gray-600 mb-2">
              <span>Success</span>
              <span>100%</span>
            </div>
            <div className="w-full bg-gray-200 rounded-full h-3">
              <div className="bg-green-600 h-3 rounded-full" style={{ width: '100%' }}></div>
            </div>
          </div>
          
          <div>
            <div className="flex justify-between text-sm text-gray-600 mb-2">
              <span>Warning</span>
              <span>75%</span>
            </div>
            <div className="w-full bg-gray-200 rounded-full h-2">
              <div className="bg-yellow-500 h-2 rounded-full" style={{ width: '75%' }}></div>
            </div>
          </div>

          <button
            onClick={simulateProgress}
            disabled={isLoading}
            className="px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {isLoading ? 'Loading...' : 'Simulate Progress'}
          </button>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Tooltips"
        codeExample={`<div className="group relative inline-block">
  <button className="px-4 py-2 bg-blue-600 text-white rounded-md">
    Hover me
  </button>
  <div className="absolute bottom-full left-1/2 transform -translate-x-1/2 mb-2 px-3 py-2 text-sm text-white bg-gray-900 rounded-lg opacity-0 group-hover:opacity-100 transition-opacity">
    This is a tooltip
  </div>
</div>`}
      >
        <div className="flex space-x-6">
          <div className="group relative inline-block">
            <button className="px-4 py-2 bg-blue-600 text-white rounded-md">
              Hover me (Top)
            </button>
            <div className="absolute bottom-full left-1/2 transform -translate-x-1/2 mb-2 px-3 py-2 text-sm text-white bg-gray-900 rounded-lg opacity-0 group-hover:opacity-100 transition-opacity pointer-events-none">
              This is a tooltip
              <div className="absolute top-full left-1/2 transform -translate-x-1/2 border-4 border-transparent border-t-gray-900"></div>
            </div>
          </div>
          
          <div className="group relative inline-block">
            <button className="px-4 py-2 bg-green-600 text-white rounded-md">
              Hover me (Bottom)
            </button>
            <div className="absolute top-full left-1/2 transform -translate-x-1/2 mt-2 px-3 py-2 text-sm text-white bg-gray-900 rounded-lg opacity-0 group-hover:opacity-100 transition-opacity pointer-events-none">
              Tooltip below
              <div className="absolute bottom-full left-1/2 transform -translate-x-1/2 border-4 border-transparent border-b-gray-900"></div>
            </div>
          </div>
          
          <div className="group relative inline-block">
            <button className="px-4 py-2 bg-purple-600 text-white rounded-md">
              Hover me (Right)
            </button>
            <div className="absolute top-1/2 left-full transform -translate-y-1/2 ml-2 px-3 py-2 text-sm text-white bg-gray-900 rounded-lg opacity-0 group-hover:opacity-100 transition-opacity pointer-events-none">
              Tooltip on right
              <div className="absolute top-1/2 right-full transform -translate-y-1/2 border-4 border-transparent border-r-gray-900"></div>
            </div>
          </div>
        </div>
      </ComponentSection>

      <ComponentSection 
        title="Status Indicators"
        codeExample={`<div className="flex items-center space-x-2">
  <div className="h-3 w-3 bg-green-500 rounded-full"></div>
  <span className="text-sm text-gray-700">Online</span>
</div>`}
      >
        <div className="space-y-4">
          <div className="flex flex-wrap gap-6">
            <div className="flex items-center space-x-2">
              <div className="h-3 w-3 bg-green-500 rounded-full"></div>
              <span className="text-sm text-gray-700">Online</span>
            </div>
            <div className="flex items-center space-x-2">
              <div className="h-3 w-3 bg-yellow-500 rounded-full"></div>
              <span className="text-sm text-gray-700">Away</span>
            </div>
            <div className="flex items-center space-x-2">
              <div className="h-3 w-3 bg-red-500 rounded-full"></div>
              <span className="text-sm text-gray-700">Offline</span>
            </div>
            <div className="flex items-center space-x-2">
              <div className="h-3 w-3 bg-gray-400 rounded-full"></div>
              <span className="text-sm text-gray-700">Unknown</span>
            </div>
          </div>
          
          <div className="flex flex-wrap gap-6">
            <div className="flex items-center space-x-2">
              <div className="h-3 w-3 bg-green-500 rounded-full animate-pulse"></div>
              <span className="text-sm text-gray-700">Active</span>
            </div>
            <div className="flex items-center space-x-2">
              <div className="relative">
                <div className="h-3 w-3 bg-blue-500 rounded-full"></div>
                <div className="absolute inset-0 bg-blue-500 rounded-full animate-ping opacity-75"></div>
              </div>
              <span className="text-sm text-gray-700">Processing</span>
            </div>
          </div>
        </div>
      </ComponentSection>
    </div>
  )

  const renderTabContent = () => {
    switch (activeTab) {
      case 'buttons': return renderButtonsTab()
      case 'forms': return renderFormsTab()
      case 'alerts': return renderAlertsTab()
      case 'tables': return renderTablesTab()
      case 'cards': return renderCardsTab()
      case 'modals': return renderModalsTab()
      case 'navigation': return renderNavigationTab()
      case 'feedback': return renderFeedbackTab()
      default: return renderButtonsTab()
    }
  }

  return (
    <div className="min-h-screen bg-gray-50 py-12">
      <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900 mb-4">Component Showcase</h1>
          <p className="text-gray-600 max-w-3xl">
            Interactive showcase of UI components built with React, Tailwind CSS, and Lucide icons. 
            Each component includes live examples and code snippets to help you understand implementation patterns.
          </p>
        </div>

        {/* Tab Navigation */}
        <div className="mb-8">
          <div className="border-b border-gray-200">
            <nav className="-mb-px flex space-x-8 overflow-x-auto">
              {tabs.map((tab) => (
                <button
                  key={tab.id}
                  onClick={() => setActiveTab(tab.id)}
                  className={clsx(
                    'flex items-center space-x-2 whitespace-nowrap border-b-2 py-4 px-1 text-sm font-medium',
                    activeTab === tab.id
                      ? 'border-blue-500 text-blue-600'
                      : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
                  )}
                >
                  {tab.icon}
                  <span>{tab.label}</span>
                </button>
              ))}
            </nav>
          </div>
        </div>

        {/* Tab Content */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-8">
          {renderTabContent()}
        </div>
      </div>
    </div>
  )
}