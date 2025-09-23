import { useState } from 'react'
import { clsx } from 'clsx'
import { 
  BookOpen, 
  Code, 
  Server, 
  Rocket,
  FileText
} from 'lucide-react'

// Import documentation components
import { GettingStarted } from './GettingStarted'
import { ApiReference } from './ApiReference'
import { CodeExamples } from './CodeExamples'
import { Deployment } from './Deployment'

type DocTab = 'getting-started' | 'api-reference' | 'code-examples' | 'deployment'

export function Documentation() {
  const [activeTab, setActiveTab] = useState<DocTab>('getting-started')

  const tabs: { id: DocTab; label: string; icon: React.ReactNode }[] = [
    { id: 'getting-started', label: 'Getting Started', icon: <BookOpen className="h-4 w-4" /> },
    { id: 'api-reference', label: 'API Reference', icon: <Server className="h-4 w-4" /> },
    { id: 'code-examples', label: 'Code Examples', icon: <Code className="h-4 w-4" /> },
    { id: 'deployment', label: 'Deployment', icon: <Rocket className="h-4 w-4" /> },
  ]

  const renderTabContent = () => {
    switch (activeTab) {
      case 'getting-started':
        return <GettingStarted />
      case 'api-reference':
        return <ApiReference />
      case 'code-examples':
        return <CodeExamples />
      case 'deployment':
        return <Deployment />
      default:
        return <GettingStarted />
    }
  }

  return (
    <div className="min-h-screen bg-gray-50 py-12">
      <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
        <div className="mb-8">
          <div className="flex items-center space-x-3 mb-4">
            <FileText className="h-8 w-8 text-blue-600" />
            <h1 className="text-3xl font-bold text-gray-900">Documentation</h1>
          </div>
          <p className="text-gray-600 max-w-3xl">
            Complete guide to building full-stack applications with React, FastAPI, and modern development tools. 
            Learn how to set up your development environment, use the API, and deploy to production.
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
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6">
          {renderTabContent()}
        </div>
      </div>
    </div>
  )
}