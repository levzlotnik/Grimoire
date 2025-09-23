import { useState, useEffect } from 'react'
import { apiClient } from '../../services/apiClient'
import { EndpointBlock } from '../../components/EndpointBlock'
import { Loader2, AlertCircle, FileText } from 'lucide-react'
import type { ApiEndpoint, ApiDocsResponse } from '../../types/api'

export function ApiReference() {
  const [apiDocs, setApiDocs] = useState<ApiDocsResponse | null>(null)
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [selectedTag, setSelectedTag] = useState<string>('all')

  useEffect(() => {
    const fetchApiDocs = async () => {
      try {
        setLoading(true)
        const docs = await apiClient.getApiEndpoints()
        setApiDocs(docs)
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Failed to load API documentation')
      } finally {
        setLoading(false)
      }
    }

    fetchApiDocs()
  }, [])

  const getMethodColor = (method: string) => {
    const colors = {
      GET: 'bg-green-100 text-green-800 border-green-200',
      POST: 'bg-blue-100 text-blue-800 border-blue-200',
      PUT: 'bg-orange-100 text-orange-800 border-orange-200',
      DELETE: 'bg-red-100 text-red-800 border-red-200',
      PATCH: 'bg-purple-100 text-purple-800 border-purple-200'
    }
    return colors[method as keyof typeof colors] || 'bg-gray-100 text-gray-800 border-gray-200'
  }

  const generateExampleRequest = (endpoint: ApiEndpoint): string => {
    const hasAuth = endpoint.security?.includes('bearerAuth')
    const hasBody = endpoint.method !== 'GET' && endpoint.requestBody
    
    // Replace path parameters with example values
    let examplePath = endpoint.path
    const pathParams = endpoint.parameters?.filter(param => param.in === 'path') || []
    
    pathParams.forEach(param => {
      const exampleValue = param.example || (param.type === 'integer' ? '1' : 'example')
      examplePath = examplePath.replace(`{${param.name}}`, exampleValue.toString())
    })
    
    let example = `curl -X ${endpoint.method} "${examplePath}" \\`
    
    if (hasAuth) {
      example += `\n  -H "Authorization: Bearer YOUR_TOKEN" \\`
    }
    
    example += `\n  -H "Content-Type: application/json"`
    
    if (hasBody && endpoint.requestBody?.content?.['application/json']?.example) {
      const bodyExample = JSON.stringify(endpoint.requestBody.content['application/json'].example, null, 2)
      example += ` \\\n  -d '${bodyExample}'`
    }
    
    return example
  }

  const generateExampleResponse = (endpoint: ApiEndpoint): string => {
    // Only use examples from the backend OpenAPI schema
    if (endpoint.examples?.response) {
      return JSON.stringify(endpoint.examples.response, null, 2)
    }
    
    // Check if responses exist and have the expected structure
    if (endpoint.responses) {
      const successResponse = endpoint.responses['200'] || endpoint.responses['201']
      if (successResponse?.content?.['application/json']?.example) {
        return JSON.stringify(successResponse.content['application/json'].example, null, 2)
      }
    }
    
    return ''
  }

  const filteredEndpoints = apiDocs?.endpoints.filter(endpoint => 
    selectedTag === 'all' || endpoint.tags.includes(selectedTag)
  ) || []

  const availableTags = Array.from(
    new Set(apiDocs?.endpoints.flatMap(endpoint => endpoint.tags) || [])
  )

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-96">
        <div className="text-center">
          <Loader2 className="h-8 w-8 animate-spin text-blue-600 mx-auto mb-4" />
          <p className="text-gray-600">Loading API documentation...</p>
        </div>
      </div>
    )
  }

  if (error) {
    return (
      <div className="bg-red-50 border border-red-200 rounded-lg p-6">
        <div className="flex items-center mb-4">
          <AlertCircle className="h-6 w-6 text-red-600 mr-3" />
          <h3 className="text-lg font-semibold text-red-800">Error Loading API Documentation</h3>
        </div>
        <p className="text-red-700">{error}</p>
        <button
          onClick={() => window.location.reload()}
          className="mt-4 bg-red-600 text-white px-4 py-2 rounded-md hover:bg-red-700 transition-colors"
        >
          Retry
        </button>
      </div>
    )
  }

  return (
    <div className="max-w-4xl mx-auto space-y-8">
      {/* Header */}
      <div className="text-center space-y-4">
        <h1 className="text-3xl font-bold text-gray-900">API Reference</h1>
        <p className="text-lg text-gray-600 max-w-2xl mx-auto">
          Complete reference for all available API endpoints. Click on any endpoint to see detailed 
          information including parameters, request/response examples, and authentication requirements.
        </p>
      </div>

      {/* Tag Filter */}
      <div className="bg-white p-6 rounded-lg border border-gray-200">
        <h3 className="text-lg font-semibold text-gray-900 mb-4">Filter by Category</h3>
        <div className="flex flex-wrap gap-2">
          <button
            onClick={() => setSelectedTag('all')}
            className={`px-4 py-2 rounded-full text-sm font-medium transition-colors ${
              selectedTag === 'all'
                ? 'bg-blue-600 text-white'
                : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
            }`}
          >
            All Endpoints ({apiDocs?.endpoints.length || 0})
          </button>
          {availableTags.map(tag => (
            <button
              key={tag}
              onClick={() => setSelectedTag(tag)}
              className={`px-4 py-2 rounded-full text-sm font-medium transition-colors ${
                selectedTag === tag
                  ? 'bg-blue-600 text-white'
                  : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
              }`}
            >
              {tag} ({apiDocs?.endpoints.filter(e => e.tags.includes(tag)).length || 0})
            </button>
          ))}
        </div>
      </div>

      {/* Endpoints */}
      <div className="space-y-6">
        {filteredEndpoints.map((endpoint, index) => (
          <EndpointBlock
            key={index}
            endpoint={endpoint}
            generateExampleRequest={generateExampleRequest}
            generateExampleResponse={generateExampleResponse}
            getMethodColor={getMethodColor}
          />
        ))}
      </div>

      {filteredEndpoints.length === 0 && (
        <div className="text-center py-12">
          <FileText className="h-12 w-12 text-gray-400 mx-auto mb-4" />
          <h3 className="text-lg font-semibold text-gray-900 mb-2">No endpoints found</h3>
          <p className="text-gray-600">
            {selectedTag === 'all' 
              ? 'No API endpoints are available.' 
              : `No endpoints found for the "${selectedTag}" category.`
            }
          </p>
        </div>
      )}
    </div>
  )
}