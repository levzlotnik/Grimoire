import { Eye, EyeOff, ChevronDown } from 'lucide-react'

interface ComponentSectionProps {
  title: string
  children: React.ReactNode
  codeExample?: string
}

interface FormData {
  name: string
  email: string
  password: string
  category: string
  newsletter: boolean
  plan: string
  message: string
}

interface FormsShowcaseProps {
  ComponentSection: React.ComponentType<ComponentSectionProps>
  formData: FormData
  setFormData: React.Dispatch<React.SetStateAction<FormData>>
  showPassword: boolean
  setShowPassword: React.Dispatch<React.SetStateAction<boolean>>
}

export function FormsShowcase({ 
  ComponentSection, 
  formData, 
  setFormData, 
  showPassword, 
  setShowPassword 
}: FormsShowcaseProps) {
  return (
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
}