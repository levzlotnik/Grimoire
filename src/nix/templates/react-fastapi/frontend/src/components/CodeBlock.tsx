import { useState } from 'react'
import { clsx } from 'clsx'
import { Copy, Check, Play, FileText } from 'lucide-react'

interface CodeBlockProps {
  code: string
  language: string
  title?: string
  output?: string
  executable?: boolean
}

export function CodeBlock({ code, language, title, output, executable = false }: CodeBlockProps) {
  const [copied, setCopied] = useState(false)
  const [isExecuting, setIsExecuting] = useState(false)
  const [executionOutput, setExecutionOutput] = useState<string | null>(null)

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(code)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    } catch (err) {
      console.error('Failed to copy code:', err)
    }
  }

  const handleExecute = async () => {
    if (!executable) return
    
    setIsExecuting(true)
    
    // Simulate code execution with different mock results based on language
    await new Promise(resolve => setTimeout(resolve, 1500))
    
    let mockOutput = ''
    if (language === 'bash' || language === 'shell') {
      mockOutput = '✓ Command executed successfully\n$ Process completed with exit code 0'
    } else if (language === 'javascript' || language === 'typescript') {
      mockOutput = '✓ Code executed successfully\n> Output: Function returned as expected'
    } else if (language === 'python') {
      mockOutput = '✓ Python script executed\n>>> Result: Operation completed successfully'
    } else if (language === 'json') {
      mockOutput = '✓ JSON validated successfully\n{ "status": "valid", "message": "No syntax errors found" }'
    } else {
      mockOutput = '✓ Code executed successfully\n> Process completed without errors'
    }
    
    setExecutionOutput(mockOutput)
    setIsExecuting(false)
  }

  const getLanguageLabel = (lang: string) => {
    const labels: Record<string, string> = {
      'javascript': 'JavaScript',
      'typescript': 'TypeScript',
      'python': 'Python',
      'bash': 'Bash',
      'shell': 'Shell',
      'json': 'JSON',
      'html': 'HTML',
      'css': 'CSS',
      'jsx': 'JSX',
      'tsx': 'TSX'
    }
    return labels[lang] || lang.toUpperCase()
  }

  const getLanguageColor = (lang: string) => {
    const colors: Record<string, string> = {
      'javascript': 'bg-yellow-100 text-yellow-800',
      'typescript': 'bg-blue-100 text-blue-800',
      'python': 'bg-green-100 text-green-800',
      'bash': 'bg-gray-100 text-gray-800',
      'shell': 'bg-gray-100 text-gray-800',
      'json': 'bg-purple-100 text-purple-800',
      'html': 'bg-orange-100 text-orange-800',
      'css': 'bg-pink-100 text-pink-800',
      'jsx': 'bg-cyan-100 text-cyan-800',
      'tsx': 'bg-indigo-100 text-indigo-800'
    }
    return colors[lang] || 'bg-gray-100 text-gray-800'
  }

  return (
    <div className="bg-white rounded-lg border border-gray-200 shadow-sm overflow-hidden">
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 bg-gray-50 border-b border-gray-200">
        <div className="flex items-center space-x-3">
          <FileText className="h-4 w-4 text-gray-500" />
          {title && (
            <span className="font-medium text-gray-900">{title}</span>
          )}
          <span className={clsx(
            'inline-flex items-center px-2 py-1 rounded-full text-xs font-medium',
            getLanguageColor(language)
          )}>
            {getLanguageLabel(language)}
          </span>
        </div>
        <div className="flex items-center space-x-2">
          {executable && (
            <button
              onClick={handleExecute}
              disabled={isExecuting}
              className={clsx(
                'inline-flex items-center px-3 py-1 rounded-md text-sm font-medium transition-colors',
                isExecuting
                  ? 'bg-gray-100 text-gray-400 cursor-not-allowed'
                  : 'bg-green-100 text-green-700 hover:bg-green-200'
              )}
            >
              <Play className={clsx('h-3 w-3 mr-1', isExecuting && 'animate-spin')} />
              {isExecuting ? 'Running...' : 'Run'}
            </button>
          )}
          <button
            onClick={handleCopy}
            className="inline-flex items-center px-3 py-1 rounded-md text-sm font-medium bg-gray-100 text-gray-700 hover:bg-gray-200 transition-colors"
          >
            {copied ? (
              <>
                <Check className="h-3 w-3 mr-1 text-green-600" />
                Copied
              </>
            ) : (
              <>
                <Copy className="h-3 w-3 mr-1" />
                Copy
              </>
            )}
          </button>
        </div>
      </div>

      {/* Code Content */}
      <div className="relative">
        <pre className="p-4 text-sm text-gray-800 bg-gray-50 overflow-x-auto">
          <code className="font-mono">{code}</code>
        </pre>
      </div>

      {/* Output Section */}
      {(output || executionOutput) && (
        <div className="border-t border-gray-200">
          <div className="px-4 py-2 bg-gray-100 border-b border-gray-200">
            <span className="text-xs font-medium text-gray-600 uppercase tracking-wide">
              Output
            </span>
          </div>
          <pre className="p-4 text-sm text-gray-700 bg-white overflow-x-auto">
            <code className="font-mono whitespace-pre-wrap">
              {executionOutput || output}
            </code>
          </pre>
        </div>
      )}
    </div>
  )
}