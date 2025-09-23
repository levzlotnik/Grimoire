import { Lock, Globe, FileText, Code } from 'lucide-react'
import { CommandWithOutputBlock } from './CommandWithOutputBlock'
import type { ApiEndpoint } from '../types/api'

interface EndpointBlockProps {
  endpoint: ApiEndpoint
  generateExampleRequest: (endpoint: ApiEndpoint) => string
  generateExampleResponse: (endpoint: ApiEndpoint) => string
  getMethodColor: (method: string) => string
}

export function EndpointBlock({ 
  endpoint, 
  generateExampleRequest, 
  generateExampleResponse, 
  getMethodColor 
}: EndpointBlockProps) {
  return (
    <div className="bg-white border border-gray-200 rounded-lg overflow-hidden">
      {/* Endpoint Header */}
      <div className="p-6 border-b border-gray-200">
        <div className="flex items-start justify-between">
          <div className="flex-1">
            <div className="flex items-center space-x-3 mb-2">
              <span className={`px-3 py-1 text-xs font-bold rounded border ${getMethodColor(endpoint.method)}`}>
                {endpoint.method}
              </span>
              <code className="text-lg font-mono text-gray-900 bg-gray-100 px-3 py-1 rounded">
                {endpoint.path}
              </code>
            </div>
            <h3 className="text-xl font-semibold text-gray-900 mb-2">{endpoint.summary}</h3>
            {endpoint.description && (
              <p className="text-gray-600">{endpoint.description}</p>
            )}
          </div>
          <div className="flex items-center space-x-2 ml-4">
            {endpoint.security?.includes('bearerAuth') ? (
              <div className="flex items-center space-x-1 text-orange-600">
                <Lock className="h-4 w-4" />
                <span className="text-sm font-medium">Auth Required</span>
              </div>
            ) : (
              <div className="flex items-center space-x-1 text-green-600">
                <Globe className="h-4 w-4" />
                <span className="text-sm font-medium">Public</span>
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Endpoint Details */}
      <div className="p-6 space-y-6">
        {/* Tags */}
        {endpoint.tags.length > 0 && (
          <div>
            <h4 className="text-sm font-semibold text-gray-900 mb-2 flex items-center">
              <FileText className="h-4 w-4 mr-2" />
              Tags
            </h4>
            <div className="flex flex-wrap gap-2">
              {endpoint.tags.map(tag => (
                <span key={tag} className="px-2 py-1 bg-gray-100 text-gray-700 text-xs rounded-full">
                  {tag}
                </span>
              ))}
            </div>
          </div>
        )}

        {/* Parameters */}
        {endpoint.parameters && endpoint.parameters.length > 0 && (
          <div>
            <h4 className="text-sm font-semibold text-gray-900 mb-3 flex items-center">
              <Code className="h-4 w-4 mr-2" />
              Parameters
            </h4>
            <div className="overflow-x-auto">
              <table className="min-w-full border border-gray-200 rounded-lg">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 uppercase">Name</th>
                    <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 uppercase">Type</th>
                    <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 uppercase">Location</th>
                    <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 uppercase">Required</th>
                    <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 uppercase">Description</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-gray-200">
                  {endpoint.parameters.map((param, paramIndex) => (
                    <tr key={paramIndex} className="hover:bg-gray-50">
                      <td className="px-4 py-2 text-sm font-mono text-gray-900">{param.name}</td>
                      <td className="px-4 py-2 text-sm text-gray-600">{param.type}</td>
                      <td className="px-4 py-2 text-sm text-gray-600">{param.in}</td>
                      <td className="px-4 py-2 text-sm">
                        {param.required ? (
                          <span className="text-red-600 font-medium">Required</span>
                        ) : (
                          <span className="text-gray-500">Optional</span>
                        )}
                      </td>
                      <td className="px-4 py-2 text-sm text-gray-600">{param.description || 'No description'}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        )}

        {/* Example Request */}
        <div>
          <h4 className="text-sm font-semibold text-gray-900 mb-3 flex items-center">
            <Code className="h-4 w-4 mr-2" />
            Example Request
          </h4>
          <CommandWithOutputBlock
            command={generateExampleRequest(endpoint)}
            language="bash"
            output={generateExampleResponse(endpoint) || undefined}
            isExampleOutput={true}
          />
        </div>
      </div>
    </div>
  )
}