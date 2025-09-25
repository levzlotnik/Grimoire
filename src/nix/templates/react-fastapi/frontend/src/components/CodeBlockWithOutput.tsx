import { useState } from 'react'
import { Copy, Check } from 'lucide-react'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { oneDark } from 'react-syntax-highlighter/dist/esm/styles/prism'
import { dark } from 'react-syntax-highlighter/dist/esm/styles/prism'

interface CodeBlockWithOutputProps {
  code: string
  language: string
  expectedOutput: string
  codeType?: 'shell' | 'repl' | 'programming'
  filePath?: string
  isExampleOutput?: boolean
}

export function CodeBlockWithOutput({
  code,
  language,
  expectedOutput,
  codeType = 'programming',
  filePath,
  isExampleOutput = false
}: CodeBlockWithOutputProps) {
  const [copied, setCopied] = useState(false)

  // Determine if it's a shell-like language
  const isShellLike = codeType === 'shell' || codeType === 'repl' ||
                     ['bash', 'shell', 'sh', 'zsh', 'fish', 'powershell', 'cmd'].includes(language.toLowerCase())

  // Choose theme based on language type
  const codeTheme = isShellLike ? dark : oneDark
  const showLineNumbers = !isShellLike

  // Generate titles
  const getCodeTitle = () => {
    if (codeType === 'shell' || codeType === 'repl') {
      return 'Command'
    }
    return 'Code'
  }

  const getCodeSubtitle = () => {
    if (codeType === 'shell') return '[shell]'
    if (codeType === 'repl') return '[repl]'
    if (filePath) return `[${filePath}]`
    return `[${language}]`
  }

  const getOutputTitle = () => {
    return isExampleOutput ? 'Example Output' : 'Output'
  }

  const handleCopy = async () => {
    await navigator.clipboard.writeText(code)
    setCopied(true)
    setTimeout(() => setCopied(false), 2000)
  }


  return (
    <div className="space-y-4">
      {/* Code Block */}
      <div className="relative">
        <div className="flex items-center justify-between bg-gray-800 text-white px-4 py-2 rounded-t-lg">
          <div className="flex flex-col">
            <span className="font-medium">{getCodeTitle()}</span>
            <span className="text-xs text-gray-300">{getCodeSubtitle()}</span>
          </div>
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

        <SyntaxHighlighter
          language={language}
          style={codeTheme}
          customStyle={{
            margin: 0,
            borderRadius: '0 0 0.5rem 0.5rem',
            fontSize: '0.875rem'
          }}
          showLineNumbers={showLineNumbers}
          wrapLongLines={true}
        >
          {code}
        </SyntaxHighlighter>
      </div>

      {/* Output Block */}
      <div className="relative">
        <div className="flex items-center justify-between bg-green-800 text-white px-4 py-2 rounded-t-lg">
          <span className="font-medium">{getOutputTitle()}</span>
          <span className="text-sm text-green-200">✓ Executed</span>
        </div>

        <SyntaxHighlighter
          language="json"
          style={oneDark}
          customStyle={{
            margin: 0,
            borderRadius: '0 0 0.5rem 0.5rem',
            fontSize: '0.875rem',
            backgroundColor: '#1a3e3a'
          }}
          showLineNumbers={false}
          wrapLongLines={true}
        >
          {expectedOutput}
        </SyntaxHighlighter>
      </div>
    </div>
  )
}