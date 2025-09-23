import { useState } from 'react'
import { clsx } from 'clsx'
import { CodeBlock } from '../../components/CodeBlock'
import {
  X,
  AlertCircle,
  Plus,
  Edit,
  Star,
  Filter,
  MessageSquare,
  Menu,
  Loader,
} from 'lucide-react'

// Import all showcase components
import { ButtonsShowcase } from './ButtonsShowcase'
import { FormsShowcase } from './FormsShowcase'
import { AlertsShowcase } from './AlertsShowcase'
import { TablesShowcase } from './TablesShowcase'
import { CardsShowcase } from './CardsShowcase'
import { ModalsShowcase } from './ModalsShowcase'
import { NavigationShowcase } from './NavigationShowcase'
import { FeedbackShowcase } from './FeedbackShowcase'

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

  const toggleCode = (title: string) => {
    setShowCode(prev => ({ ...prev, [title]: !prev[title] }))
  }

  // ComponentSection helper
  const ComponentSection = ({ 
    title, 
    children, 
    codeExample 
  }: {
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

  const renderTabContent = () => {
    switch (activeTab) {
      case 'buttons': 
        return <ButtonsShowcase ComponentSection={ComponentSection} />
      case 'forms': 
        return <FormsShowcase 
          ComponentSection={ComponentSection} 
          formData={formData} 
          setFormData={setFormData} 
          showPassword={showPassword} 
          setShowPassword={setShowPassword} 
        />
      case 'alerts': 
        return <AlertsShowcase 
          ComponentSection={ComponentSection} 
          alerts={alerts} 
          setAlerts={setAlerts} 
        />
      case 'tables': 
        return <TablesShowcase 
          ComponentSection={ComponentSection} 
          tableData={tableData} 
          sortField={sortField} 
          setSortField={setSortField} 
          sortDirection={sortDirection} 
          setSortDirection={setSortDirection} 
          currentPage={currentPage} 
          setCurrentPage={setCurrentPage} 
        />
      case 'cards': 
        return <CardsShowcase ComponentSection={ComponentSection} />
      case 'modals': 
        return <ModalsShowcase 
          ComponentSection={ComponentSection} 
          modals={modals} 
          setModals={setModals} 
        />
      case 'navigation': 
        return <NavigationShowcase 
          ComponentSection={ComponentSection} 
          currentBreadcrumb={currentBreadcrumb} 
          setCurrentBreadcrumb={setCurrentBreadcrumb} 
          navTab={navTab} 
          setNavTab={setNavTab} 
        />
      case 'feedback': 
        return <FeedbackShowcase 
          ComponentSection={ComponentSection} 
          progress={progress} 
          setProgress={setProgress} 
          isLoading={isLoading} 
          setIsLoading={setIsLoading} 
        />
      default: 
        return <ButtonsShowcase ComponentSection={ComponentSection} />
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