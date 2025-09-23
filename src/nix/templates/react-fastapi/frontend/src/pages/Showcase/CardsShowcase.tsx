import { Heart, CheckCircle, Star } from 'lucide-react'

interface ComponentSectionProps {
  title: string
  children: React.ReactNode
  codeExample?: string
}

interface CardsShowcaseProps {
  ComponentSection: React.ComponentType<ComponentSectionProps>
}

export function CardsShowcase({ ComponentSection }: CardsShowcaseProps) {
  return (
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
}