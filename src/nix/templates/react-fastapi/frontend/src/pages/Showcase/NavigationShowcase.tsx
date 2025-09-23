import { ChevronRight } from 'lucide-react'
import { clsx } from 'clsx'

interface ComponentSectionProps {
  title: string
  children: React.ReactNode
  codeExample?: string
}

interface NavigationShowcaseProps {
  ComponentSection: React.ComponentType<ComponentSectionProps>
  currentBreadcrumb: string[]
  setCurrentBreadcrumb: (breadcrumb: string[]) => void
  navTab: string
  setNavTab: (tab: string) => void
  currentPage: number
  setCurrentPage: (page: number) => void
}

export function NavigationShowcase({
  ComponentSection,
  currentBreadcrumb,
  setCurrentBreadcrumb,
  navTab,
  setNavTab,
  currentPage,
  setCurrentPage
}: NavigationShowcaseProps) {
  return (
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
              onClick={() => setCurrentPage(Math.max(1, currentPage - 1))}
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
              onClick={() => setCurrentPage(Math.min(3, currentPage + 1))}
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
}