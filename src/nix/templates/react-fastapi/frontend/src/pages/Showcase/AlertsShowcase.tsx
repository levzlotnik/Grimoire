import { CheckCircle, XCircle, AlertCircle, Info, X } from 'lucide-react'

interface ComponentSectionProps {
  title: string
  children: React.ReactNode
  codeExample?: string
}

interface AlertsShowcaseProps {
  ComponentSection: React.ComponentType<ComponentSectionProps>
  alerts: {
    success: boolean
    error: boolean
    warning: boolean
    info: boolean
  }
  setAlerts: React.Dispatch<React.SetStateAction<{
    success: boolean
    error: boolean
    warning: boolean
    info: boolean
  }>>
}

export function AlertsShowcase({ ComponentSection, alerts, setAlerts }: AlertsShowcaseProps) {
  return (
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
}