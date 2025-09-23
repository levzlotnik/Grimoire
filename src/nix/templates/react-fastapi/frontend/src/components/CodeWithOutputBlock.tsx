import { useState } from 'react'
import { Copy, Check } from 'lucide-react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { oneDark } from 'react-syntax-highlighter/dist/esm/styles/prism'

interface CodeWithOutputBlockProps {
  code: string
  language: string
  filePath: string
  output?: string
  isExampleOutput?: boolean
  startLine?: number
  success?: boolean
}

export function CodeWithOutputBlock({ 
  code, 
  language,
  filePath,
  output,
  isExampleOutput = false,
  startLine = 1,
  success = true
}: CodeWithOutputBlockProps) {
  const [copied, setCopied] = useState(false)

  const handleCopy = async () => {
    await navigator.clipboard.writeText(code)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }

  const colorizeOutput = (text: string) => {
    const lines = text.split('\n')
    return lines.map((line, index) => {
      const lowerLine = line.toLowerCase()
      let className = 'text-white'
      
      if (lowerLine.includes('error') || lowerLine.includes('fatal') || lowerLine.includes('critical')) {
        className = 'text-red-400'
      } else if (lowerLine.includes('warning')) {
        className = 'text-yellow-400'
      }
      
      return (
        <div key={index} className={className}>
          {line}
        </div>
      )
    })
  }

  return (
    <div style={{ width: 'fit-content', maxWidth: '120ch' }}>
      {/* Code Block */}
      <div className="relative">
        <div className="bg-gray-800 text-white px-4 py-2 rounded-t-lg">
          <div className="flex items-center justify-between">
          <span className="font-medium">
            <code className="bg-gray-700 px-2 py-1 rounded text-sm text-gray-300">{filePath}</code>
          </span>
          <div className="flex items-center space-x-3">
            <span className="text-xs text-gray-400 uppercase tracking-wide font-medium">
              {language}
            </span>
            <button
              onClick={handleCopy}
              className="flex items-center space-x-1 text-gray-300 hover:text-white transition-colors"
            >
              {copied ? (
                <>
                  <Check className="h-4 w-4" />
                  <span className="text-sm">Copied!</span>
                </>
              ) : (
                <>
                  <Copy className="h-4 w-4" />
                  <span className="text-sm">Copy</span>
                </>
              )}
            </button>
          </div>
          </div>
        </div>
        
        <SyntaxHighlighter
          language={language}
          style={oneDark}
          customStyle={{
            margin: 0,
            borderRadius: output ? '0' : '0 0 0.5rem 0.5rem',
            fontSize: '0.875rem',
            maxHeight: '400px',
            overflowY: 'auto',
            overflowX: 'auto'
          }}
          showLineNumbers={true}
          startingLineNumber={startLine}
          wrapLongLines={false}
        >
          {code}
        </SyntaxHighlighter>
      </div>

      {/* Output Block */}
      {output && (
        <div className="relative">
          <div className={`${success ? 'bg-green-800' : 'bg-red-800'} text-white px-4 py-2`}>
            <div className="flex items-center justify-between">
            <span className="font-medium">{isExampleOutput ? 'Example Output' : 'Output'}</span>
            <span className={`text-sm ${success ? 'text-green-200' : 'text-red-200'}`}>
              {success ? '✓ Executed' : '✗ Failed'}
            </span>
            </div>
          </div>
          
          <div 
            className="bg-black text-white p-4 font-mono text-sm rounded-b-lg overflow-auto"
            style={{
              maxHeight: '300px',
              fontSize: '0.875rem'
            }}
          >
            {colorizeOutput(output)}
          </div>
        </div>
      )}
    </div>
  )
}