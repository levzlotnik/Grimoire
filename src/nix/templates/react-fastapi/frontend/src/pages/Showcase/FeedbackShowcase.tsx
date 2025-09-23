import { Loader } from 'lucide-react'

interface ComponentSectionProps {
  title: string
  children: React.ReactNode
  codeExample?: string
}

interface FeedbackShowcaseProps {
  ComponentSection: React.ComponentType<ComponentSectionProps>
  progress: number
  setProgress: (value: number | ((prev: number) => number)) => void
  isLoading: boolean
  setIsLoading: (value: boolean) => void
}

export function FeedbackShowcase({ 
  ComponentSection, 
  progress, 
  setProgress, 
  isLoading, 
  setIsLoading 
}: FeedbackShowcaseProps) {
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

  return (
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
}