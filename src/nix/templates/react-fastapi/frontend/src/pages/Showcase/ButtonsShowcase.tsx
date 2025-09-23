import React from 'react'
import {
  Heart,
  Star,
  Bell,
  Settings,
  Download,
  Upload,
  Edit,
  Trash2,
  MoreVertical
} from 'lucide-react'

interface ComponentSectionProps {
  title: string
  children: React.ReactNode
  codeExample?: string
}

interface ButtonsShowcaseProps {
  ComponentSection: React.ComponentType<ComponentSectionProps>
}

export function ButtonsShowcase({ ComponentSection }: ButtonsShowcaseProps) {
  return (
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
}