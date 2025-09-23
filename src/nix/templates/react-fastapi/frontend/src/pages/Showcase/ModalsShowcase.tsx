import { X, AlertCircle } from 'lucide-react'

interface ComponentSectionProps {
  title: string
  children: React.ReactNode
  codeExample?: string
}

interface ModalsShowcaseProps {
  ComponentSection: React.ComponentType<ComponentSectionProps>
  modals: {
    basic: boolean
    confirmation: boolean
    form: boolean
  }
  setModals: React.Dispatch<React.SetStateAction<{
    basic: boolean
    confirmation: boolean
    form: boolean
  }>>
}

export function ModalsShowcase({ ComponentSection, modals, setModals }: ModalsShowcaseProps) {
  return (
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
}