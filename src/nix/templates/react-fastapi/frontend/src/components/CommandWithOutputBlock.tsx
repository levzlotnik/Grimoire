import { useState } from 'react'
import { Copy, Check } from 'lucide-react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { vscDarkPlus } from 'react-syntax-highlighter/dist/esm/styles/prism'

interface CommandWithOutputBlockProps {
  command: string
  language: 'bash' | 'shell' | 'zsh' | 'fish' | 'powershell' | 'cmd'
  output?: string
  isExampleOutput?: boolean
  success?: boolean
}

export function CommandWithOutputBlock({ 
  command, 
  language,
  output,
  isExampleOutput = false,
  success = true
}: CommandWithOutputBlockProps) {
  const [copied, setCopied] = useState(false)

  const handleCopy = async () => {
    await navigator.clipboard.writeText(command)
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

  const getLanguageTitle = () => {
    const titles: Record<string, string> = {
      'bash': 'Bash Command',
      'shell': 'Shell Command', 
      'zsh': 'Zsh Command',
      'fish': 'Fish Command',
      'powershell': 'PowerShell Command',
      'cmd': 'CMD Command'
    }
    return titles[language] || 'Shell Command'
  }

  return (
    <div style={{ width: 'fit-content', maxWidth: '120ch' }}>
      {/* Command Block */}
      <div className="relative">
        <div className="bg-gray-800 text-white px-4 py-2 rounded-t-lg">
          <div className="flex items-center justify-between">
          <span className="font-medium">{getLanguageTitle()}</span>
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
        
        <SyntaxHighlighter
          language={language}
          style={vscDarkPlus}
          customStyle={{
            margin: 0,
            borderRadius: output ? '0' : '0 0 0.5rem 0.5rem',
            fontSize: '0.875rem',
            backgroundColor: '#1e1e1e',
            maxHeight: '400px',
            overflowY: 'auto',
            overflowX: 'auto'
          }}
          showLineNumbers={false}
          wrapLongLines={false}
        >
          {command}
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