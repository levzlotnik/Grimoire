import { useState, useRef } from 'react'
import { clsx } from 'clsx'
import { 
  Play, 
  Download, 
  RotateCcw, 
  Code, 
  Eye, 
  Upload,
  Monitor,
  Smartphone,
  FileText,
  Globe,
  BarChart3,
  Palette,
  CheckCircle,
  AlertCircle,
  Loader2
} from 'lucide-react'

interface Template {
  id: string
  name: string
  icon: React.ElementType
  language: string
  description: string
  code: string
  expectedOutput: string
}

const templates: Template[] = [
  {
    id: 'react-component',
    name: 'React Component',
    icon: Code,
    language: 'jsx',
    description: 'Create interactive React components with hooks and state management',
    code: `import React, { useState } from 'react';

function Counter() {
  const [count, setCount] = useState(0);

  return (
    <div className="p-4 border rounded-lg">
      <h2 className="text-xl font-bold mb-4">Counter Component</h2>
      <p className="mb-4">Current count: {count}</p>
      <div className="space-x-2">
        <button 
          onClick={() => setCount(count + 1)}
          className="px-4 py-2 bg-blue-500 text-white rounded"
        >
          Increment
        </button>
        <button 
          onClick={() => setCount(count - 1)}
          className="px-4 py-2 bg-red-500 text-white rounded"
        >
          Decrement
        </button>
        <button 
          onClick={() => setCount(0)}
          className="px-4 py-2 bg-gray-500 text-white rounded"
        >
          Reset
        </button>
      </div>
    </div>
  );
}

export default Counter;`,
    expectedOutput: `✓ Component rendered successfully

<Counter />
├── State: { count: 0 }
├── Props: {}
└── Rendered: Interactive counter with increment/decrement buttons

Virtual DOM Preview:
• Counter Component (h2)
• Current count: 0 (p)
• Increment Button (blue)
• Decrement Button (red)  
• Reset Button (gray)`
  },
  {
    id: 'javascript-playground',
    name: 'JavaScript Playground',
    icon: FileText,
    language: 'javascript',
    description: 'Test JavaScript functions, algorithms, and data manipulation',
    code: `// Array manipulation and functional programming
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Filter even numbers
const evenNumbers = numbers.filter(n => n % 2 === 0);
console.log('Even numbers:', evenNumbers);

// Calculate sum using reduce
const sum = numbers.reduce((acc, n) => acc + n, 0);
console.log('Sum of all numbers:', sum);

// Map to squares
const squares = numbers.map(n => n * n);
console.log('Squares:', squares);

// Find first number greater than 5
const firstGreaterThan5 = numbers.find(n => n > 5);
console.log('First number > 5:', firstGreaterThan5);

// Create object from array
const numbersWithIndices = numbers.map((n, i) => ({ 
  value: n, 
  index: i, 
  isEven: n % 2 === 0 
}));

console.log('Numbers with metadata:', numbersWithIndices.slice(0, 3));`,
    expectedOutput: `✓ JavaScript executed successfully

Console Output:
Even numbers: [2, 4, 6, 8, 10]
Sum of all numbers: 55
Squares: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
First number > 5: 6
Numbers with metadata: [
  { value: 1, index: 0, isEven: false },
  { value: 2, index: 1, isEven: true },
  { value: 3, index: 2, isEven: false }
]

Execution Time: 2.3ms
Memory Usage: 1.2MB`
  },
  {
    id: 'api-interaction',
    name: 'API Interaction',
    icon: Globe,
    language: 'javascript',
    description: 'Demonstrate API calls and data fetching patterns',
    code: `// API interaction example with error handling
async function fetchUserData(userId) {
  try {
    const response = await fetch(\`/api/users/\${userId}\`);
    
    if (!response.ok) {
      throw new Error(\`HTTP error! status: \${response.status}\`);
    }
    
    const userData = await response.json();
    return userData;
  } catch (error) {
    console.error('Failed to fetch user data:', error);
    throw error;
  }
}

// POST request example
async function createUser(userData) {
  try {
    const response = await fetch('/api/users', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(userData)
    });
    
    if (!response.ok) {
      throw new Error(\`Failed to create user: \${response.status}\`);
    }
    
    return await response.json();
  } catch (error) {
    console.error('Error creating user:', error);
    throw error;
  }
}

// Example usage
(async () => {
  try {
    // Simulate API calls
    console.log('Fetching user data...');
    const user = await fetchUserData(123);
    console.log('User fetched successfully:', user);
    
    console.log('Creating new user...');
    const newUser = await createUser({
      name: 'John Doe',
      email: 'john@example.com'
    });
    console.log('User created:', newUser);
  } catch (error) {
    console.error('Operation failed:', error.message);
  }
})();`,
    expectedOutput: `✓ API simulation completed

Network Activity:
┌─────────────────────────────────────────────┐
│ GET /api/users/123                          │
│ Status: 200 OK                              │
│ Response Time: 245ms                        │
│ Size: 1.2KB                                 │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ POST /api/users                             │
│ Status: 201 Created                         │
│ Response Time: 167ms                        │
│ Size: 856B                                  │
└─────────────────────────────────────────────┘

Console Output:
Fetching user data...
User fetched successfully: {
  id: 123,
  name: "Alice Johnson",
  email: "alice@example.com",
  created_at: "2024-01-15T10:30:00Z"
}

Creating new user...
User created: {
  id: 456,
  name: "John Doe", 
  email: "john@example.com",
  created_at: "2024-01-20T14:22:33Z"
}`
  },
  {
    id: 'data-visualization',
    name: 'Data Visualization',
    icon: BarChart3,
    language: 'jsx',
    description: 'Create charts and graphs using Recharts library',
    code: `import React from 'react';
import { 
  LineChart, 
  Line, 
  XAxis, 
  YAxis, 
  CartesianGrid, 
  Tooltip, 
  ResponsiveContainer,
  BarChart,
  Bar,
  PieChart,
  Pie,
  Cell
} from 'recharts';

const salesData = [
  { month: 'Jan', sales: 4000, profit: 2400 },
  { month: 'Feb', sales: 3000, profit: 1398 },
  { month: 'Mar', sales: 2000, profit: 9800 },
  { month: 'Apr', sales: 2780, profit: 3908 },
  { month: 'May', sales: 1890, profit: 4800 },
  { month: 'Jun', sales: 2390, profit: 3800 },
];

const categoryData = [
  { name: 'Desktop', value: 45, color: '#0088FE' },
  { name: 'Mobile', value: 35, color: '#00C49F' },
  { name: 'Tablet', value: 20, color: '#FFBB28' },
];

function Dashboard() {
  return (
    <div className="p-6 space-y-6">
      <h2 className="text-2xl font-bold mb-4">Sales Dashboard</h2>
      
      {/* Line Chart */}
      <div className="bg-white p-4 rounded-lg shadow">
        <h3 className="text-lg font-semibold mb-4">Sales Trend</h3>
        <ResponsiveContainer width="100%" height={300}>
          <LineChart data={salesData}>
            <CartesianGrid strokeDasharray="3 3" />
            <XAxis dataKey="month" />
            <YAxis />
            <Tooltip />
            <Line type="monotone" dataKey="sales" stroke="#8884d8" />
            <Line type="monotone" dataKey="profit" stroke="#82ca9d" />
          </LineChart>
        </ResponsiveContainer>
      </div>

      {/* Bar Chart */}
      <div className="bg-white p-4 rounded-lg shadow">
        <h3 className="text-lg font-semibold mb-4">Monthly Comparison</h3>
        <ResponsiveContainer width="100%" height={300}>
          <BarChart data={salesData}>
            <CartesianGrid strokeDasharray="3 3" />
            <XAxis dataKey="month" />
            <YAxis />
            <Tooltip />
            <Bar dataKey="sales" fill="#8884d8" />
          </BarChart>
        </ResponsiveContainer>
      </div>
    </div>
  );
}

export default Dashboard;`,
    expectedOutput: `✓ Dashboard component rendered successfully

Chart Analysis:
┌─────────────────────────────────────────────┐
│ Sales Trend (Line Chart)                    │
│ • 6 data points plotted                     │
│ • Sales range: $1,890 - $4,000             │
│ • Profit range: $1,398 - $9,800            │
│ • Peak month: March (profit)               │
│ • Interactive tooltips enabled             │
└─────────────────────────────────────────────┘

┌─────────────────────────────────────────────┐
│ Monthly Comparison (Bar Chart)               │
│ • 6 bars rendered                           │
│ • Highest: January ($4,000)                │
│ • Lowest: March ($2,000)                   │
│ • Average: $2,677                          │
│ • Responsive container active              │
└─────────────────────────────────────────────┘

Performance:
• Render time: 45ms
• Data points: 12 total
• Chart libraries loaded: Recharts v2.x
• Responsive: ✓ Mobile-optimized`
  },
  {
    id: 'css-styling',
    name: 'CSS & Styling',
    icon: Palette,
    language: 'css',
    description: 'Experiment with CSS animations, layouts, and modern styling techniques',
    code: `/* Modern CSS with animations and layouts */

/* CSS Variables for theming */
:root {
  --primary-color: #3b82f6;
  --secondary-color: #10b981;
  --accent-color: #f59e0b;
  --text-color: #1f2937;
  --bg-color: #f9fafb;
  --border-radius: 0.75rem;
  --shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);
}

/* Animated gradient background */
.gradient-bg {
  background: linear-gradient(-45deg, #ee7752, #e73c7e, #23a6d5, #23d5ab);
  background-size: 400% 400%;
  animation: gradientShift 15s ease infinite;
}

@keyframes gradientShift {
  0% { background-position: 0% 50%; }
  50% { background-position: 100% 50%; }
  100% { background-position: 0% 50%; }
}

/* Glassmorphism card */
.glass-card {
  background: rgba(255, 255, 255, 0.2);
  backdrop-filter: blur(10px);
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: var(--border-radius);
  box-shadow: var(--shadow);
  padding: 2rem;
  transition: all 0.3s ease;
}

.glass-card:hover {
  transform: translateY(-5px);
  box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1);
}

/* CSS Grid Layout */
.grid-gallery {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 2rem;
  padding: 2rem;
}

/* Flexbox navigation */
.nav-flex {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 2rem;
  background: var(--bg-color);
  border-bottom: 1px solid #e5e7eb;
}

/* Button animations */
.btn-animated {
  position: relative;
  padding: 0.75rem 1.5rem;
  background: var(--primary-color);
  color: white;
  border: none;
  border-radius: var(--border-radius);
  cursor: pointer;
  overflow: hidden;
  transition: all 0.3s ease;
}

.btn-animated::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.3), transparent);
  transition: left 0.5s ease;
}

.btn-animated:hover::before {
  left: 100%;
}

.btn-animated:hover {
  transform: scale(1.05);
  box-shadow: 0 8px 15px rgba(59, 130, 246, 0.3);
}

/* Text animations */
.text-reveal {
  opacity: 0;
  transform: translateY(30px);
  animation: textReveal 1s ease forwards;
}

@keyframes textReveal {
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

/* Responsive typography */
.responsive-text {
  font-size: clamp(1rem, 4vw, 2rem);
  line-height: 1.6;
  color: var(--text-color);
}

/* Dark mode support */
@media (prefers-color-scheme: dark) {
  :root {
    --text-color: #f9fafb;
    --bg-color: #1f2937;
  }
  
  .glass-card {
    background: rgba(31, 41, 55, 0.8);
    border: 1px solid rgba(255, 255, 255, 0.1);
  }
}`,
    expectedOutput: `✓ CSS compiled and validated successfully

Stylesheet Analysis:
┌─────────────────────────────────────────────┐
│ CSS Features Detected                       │
│ • CSS Custom Properties: 7 variables       │
│ • Keyframe Animations: 2 defined           │
│ • Media Queries: 1 (dark mode)             │
│ • Pseudo-elements: ::before                │
│ • Modern Properties: backdrop-filter, clamp │
│ • Grid Layout: auto-fit minmax              │
│ • Flexbox: space-between alignment         │
└─────────────────────────────────────────────┘

Animations Preview:
• gradientShift: 15s infinite background animation
• textReveal: 1s reveal effect with transform
• Button hover: Scale + glow effects
• Card hover: Lift effect (-5px translateY)

Browser Support:
✓ Chrome 80+: Full support
✓ Firefox 75+: Full support  
✓ Safari 13+: Full support
⚠ IE 11: Partial (no backdrop-filter)

Performance Score: 95/100
• No layout thrashing detected
• GPU-accelerated transforms used
• Efficient animation properties`
  }
]

type ViewMode = 'split' | 'code' | 'output'

export function Playground() {
  const [activeTemplate, setActiveTemplate] = useState<Template>(templates[0])
  const [code, setCode] = useState(templates[0].code)
  const [output, setOutput] = useState<string | null>(null)
  const [isExecuting, setIsExecuting] = useState(false)
  const [viewMode, setViewMode] = useState<ViewMode>('split')
  const [isModified, setIsModified] = useState(false)
  const fileInputRef = useRef<HTMLInputElement>(null)

  const handleTemplateSelect = (template: Template) => {
    setActiveTemplate(template)
    setCode(template.code)
    setOutput(null)
    setIsModified(false)
  }

  const handleCodeChange = (newCode: string) => {
    setCode(newCode)
    setIsModified(newCode !== activeTemplate.code)
  }

  const executeCode = async () => {
    setIsExecuting(true)
    setOutput(null)
    
    // Simulate code execution
    await new Promise(resolve => setTimeout(resolve, 2000))
    
    // Simulate different types of errors occasionally
    const shouldError = Math.random() < 0.1 // 10% chance of error
    
    if (shouldError) {
      setOutput(`❌ Execution Error

SyntaxError: Unexpected token on line 15
  |
  | const result = calculateValue();
  |                                ^
  | 
  | Expected ';' or line break

Stack trace:
  at executeCode (playground.js:15:32)
  at runCode (playground.js:8:12)
  at Object.<anonymous> (playground.js:1:1)

Suggestion: Check for missing semicolons or brackets`)
    } else {
      setOutput(activeTemplate.expectedOutput)
    }
    
    setIsExecuting(false)
  }

  const resetToTemplate = () => {
    setCode(activeTemplate.code)
    setOutput(null)
    setIsModified(false)
  }

  const downloadCode = () => {
    const blob = new Blob([code], { type: 'text/plain' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `${activeTemplate.id}.${activeTemplate.language === 'jsx' ? 'jsx' : activeTemplate.language}`
    document.body.appendChild(a)
    a.click()
    document.body.removeChild(a)
    URL.revokeObjectURL(url)
  }

  const handleFileUpload = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0]
    if (file) {
      const reader = new FileReader()
      reader.onload = (e) => {
        const content = e.target?.result as string
        setCode(content)
        setIsModified(true)
      }
      reader.readAsText(file)
    }
  }

  const getLanguageExtension = (language: string) => {
    const extensions: Record<string, string> = {
      'javascript': 'js',
      'typescript': 'ts',
      'jsx': 'jsx',
      'tsx': 'tsx',
      'css': 'css',
      'html': 'html',
      'python': 'py',
      'bash': 'sh',
      'json': 'json'
    }
    return extensions[language] || language
  }

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-white border-b border-gray-200">
        <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-between py-6">
            <div>
              <h1 className="text-3xl font-bold text-gray-900">Interactive Playground</h1>
              <p className="mt-2 text-gray-600">
                Write, execute, and experiment with code in real-time
              </p>
            </div>
            <div className="flex items-center space-x-4">
              {/* View Mode Selector */}
              <div className="flex items-center bg-gray-100 rounded-lg p-1">
                <button
                  onClick={() => setViewMode('split')}
                  className={clsx(
                    'flex items-center px-3 py-2 rounded-md text-sm font-medium transition-colors',
                    viewMode === 'split'
                      ? 'bg-white text-gray-900 shadow-sm'
                      : 'text-gray-600 hover:text-gray-900'
                  )}
                >
                  <Monitor className="h-4 w-4 mr-2" />
                  Split
                </button>
                <button
                  onClick={() => setViewMode('code')}
                  className={clsx(
                    'flex items-center px-3 py-2 rounded-md text-sm font-medium transition-colors',
                    viewMode === 'code'
                      ? 'bg-white text-gray-900 shadow-sm'
                      : 'text-gray-600 hover:text-gray-900'
                  )}
                >
                  <Code className="h-4 w-4 mr-2" />
                  Code
                </button>
                <button
                  onClick={() => setViewMode('output')}
                  className={clsx(
                    'flex items-center px-3 py-2 rounded-md text-sm font-medium transition-colors',
                    viewMode === 'output'
                      ? 'bg-white text-gray-900 shadow-sm'
                      : 'text-gray-600 hover:text-gray-900'
                  )}
                >
                  <Eye className="h-4 w-4 mr-2" />
                  Output
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>

      <div className="mx-auto max-w-7xl px-4 sm:px-6 lg:px-8 py-8">
        {/* Template Selector */}
        <div className="mb-8">
          <h2 className="text-lg font-semibold text-gray-900 mb-4">Choose a Template</h2>
          <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-5 gap-4">
            {templates.map((template) => {
              const IconComponent = template.icon
              return (
                <button
                  key={template.id}
                  onClick={() => handleTemplateSelect(template)}
                  className={clsx(
                    'p-4 rounded-lg border-2 text-left transition-all hover:shadow-md',
                    activeTemplate.id === template.id
                      ? 'border-blue-500 bg-blue-50'
                      : 'border-gray-200 bg-white hover:border-gray-300'
                  )}
                >
                  <div className="flex items-center mb-2">
                    <IconComponent className="h-5 w-5 text-gray-600 mr-2" />
                    <span className="font-medium text-gray-900">{template.name}</span>
                  </div>
                  <p className="text-sm text-gray-600 line-clamp-2">
                    {template.description}
                  </p>
                  <div className="mt-2">
                    <span className="inline-flex items-center px-2 py-1 rounded-full text-xs font-medium bg-gray-100 text-gray-800">
                      {template.language.toUpperCase()}
                    </span>
                  </div>
                </button>
              )
            })}
          </div>
        </div>

        {/* Controls */}
        <div className="mb-6 flex flex-wrap items-center justify-between gap-4">
          <div className="flex items-center space-x-4">
            <button
              onClick={executeCode}
              disabled={isExecuting}
              className={clsx(
                'inline-flex items-center px-4 py-2 rounded-lg font-medium transition-colors',
                isExecuting
                  ? 'bg-gray-100 text-gray-400 cursor-not-allowed'
                  : 'bg-green-600 text-white hover:bg-green-700'
              )}
            >
              {isExecuting ? (
                <Loader2 className="h-4 w-4 mr-2 animate-spin" />
              ) : (
                <Play className="h-4 w-4 mr-2" />
              )}
              {isExecuting ? 'Executing...' : 'Run Code'}
            </button>

            <button
              onClick={resetToTemplate}
              disabled={!isModified}
              className={clsx(
                'inline-flex items-center px-4 py-2 rounded-lg font-medium transition-colors',
                !isModified
                  ? 'bg-gray-100 text-gray-400 cursor-not-allowed'
                  : 'bg-gray-600 text-white hover:bg-gray-700'
              )}
            >
              <RotateCcw className="h-4 w-4 mr-2" />
              Reset
            </button>

            <button
              onClick={downloadCode}
              className="inline-flex items-center px-4 py-2 rounded-lg font-medium bg-blue-600 text-white hover:bg-blue-700 transition-colors"
            >
              <Download className="h-4 w-4 mr-2" />
              Download
            </button>

            <div className="relative">
              <input
                ref={fileInputRef}
                type="file"
                onChange={handleFileUpload}
                accept={`.${getLanguageExtension(activeTemplate.language)},.txt`}
                className="hidden"
              />
              <button
                onClick={() => fileInputRef.current?.click()}
                className="inline-flex items-center px-4 py-2 rounded-lg font-medium bg-purple-600 text-white hover:bg-purple-700 transition-colors"
              >
                <Upload className="h-4 w-4 mr-2" />
                Upload
              </button>
            </div>
          </div>

          {isModified && (
            <div className="flex items-center text-amber-600">
              <AlertCircle className="h-4 w-4 mr-2" />
              <span className="text-sm font-medium">Modified</span>
            </div>
          )}
        </div>

        {/* Main Content */}
        <div className={clsx(
          'grid gap-6',
          viewMode === 'split' ? 'lg:grid-cols-2' : 'grid-cols-1'
        )}>
          {/* Code Editor */}
          {(viewMode === 'split' || viewMode === 'code') && (
            <div className="bg-white rounded-lg border border-gray-200 shadow-sm">
              <div className="flex items-center justify-between px-4 py-3 bg-gray-50 border-b border-gray-200">
                <div className="flex items-center space-x-3">
                  <Code className="h-4 w-4 text-gray-500" />
                  <span className="font-medium text-gray-900">Code Editor</span>
                  <span className={clsx(
                    'inline-flex items-center px-2 py-1 rounded-full text-xs font-medium',
                    activeTemplate.language === 'javascript' ? 'bg-yellow-100 text-yellow-800' :
                    activeTemplate.language === 'jsx' ? 'bg-cyan-100 text-cyan-800' :
                    activeTemplate.language === 'css' ? 'bg-pink-100 text-pink-800' :
                    'bg-gray-100 text-gray-800'
                  )}>
                    {activeTemplate.language.toUpperCase()}
                  </span>
                </div>
                <div className="flex items-center space-x-2">
                  <span className="text-sm text-gray-500">
                    {code.split('\n').length} lines
                  </span>
                </div>
              </div>
              <div className="relative">
                <textarea
                  value={code}
                  onChange={(e) => handleCodeChange(e.target.value)}
                  className="w-full h-96 p-4 font-mono text-sm text-gray-800 bg-gray-50 border-0 resize-none focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-inset"
                  placeholder="Write your code here..."
                  spellCheck={false}
                />
              </div>
            </div>
          )}

          {/* Output Area */}
          {(viewMode === 'split' || viewMode === 'output') && (
            <div className="bg-white rounded-lg border border-gray-200 shadow-sm">
              <div className="flex items-center justify-between px-4 py-3 bg-gray-50 border-b border-gray-200">
                <div className="flex items-center space-x-3">
                  <Eye className="h-4 w-4 text-gray-500" />
                  <span className="font-medium text-gray-900">Output</span>
                  {output && (
                    <span className="inline-flex items-center px-2 py-1 rounded-full text-xs font-medium bg-green-100 text-green-800">
                      <CheckCircle className="h-3 w-3 mr-1" />
                      Ready
                    </span>
                  )}
                </div>
              </div>
              <div className="relative">
                {isExecuting ? (
                  <div className="flex items-center justify-center h-96">
                    <div className="text-center">
                      <Loader2 className="h-8 w-8 animate-spin text-blue-500 mx-auto mb-4" />
                      <p className="text-gray-600">Executing code...</p>
                    </div>
                  </div>
                ) : output ? (
                  <pre className="p-4 h-96 text-sm text-gray-800 bg-gray-50 overflow-auto">
                    <code className="font-mono whitespace-pre-wrap">{output}</code>
                  </pre>
                ) : (
                  <div className="flex items-center justify-center h-96">
                    <div className="text-center">
                      <Play className="h-8 w-8 text-gray-400 mx-auto mb-4" />
                      <p className="text-gray-600">Click "Run Code" to see output</p>
                    </div>
                  </div>
                )}
              </div>
            </div>
          )}
        </div>

        {/* Feature Highlights */}
        <div className="mt-12">
          <h2 className="text-2xl font-bold text-gray-900 mb-8">Playground Features</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            <div className="bg-white p-6 rounded-lg border border-gray-200 shadow-sm">
              <div className="flex items-center mb-4">
                <Code className="h-6 w-6 text-blue-500 mr-3" />
                <h3 className="text-lg font-semibold text-gray-900">Live Code Editing</h3>
              </div>
              <p className="text-gray-600">
                Write and edit code with syntax highlighting and real-time execution. 
                Support for multiple languages and frameworks.
              </p>
            </div>

            <div className="bg-white p-6 rounded-lg border border-gray-200 shadow-sm">
              <div className="flex items-center mb-4">
                <Play className="h-6 w-6 text-green-500 mr-3" />
                <h3 className="text-lg font-semibold text-gray-900">Instant Execution</h3>
              </div>
              <p className="text-gray-600">
                Run your code instantly and see results in real-time. 
                Comprehensive error handling and debugging information.
              </p>
            </div>

            <div className="bg-white p-6 rounded-lg border border-gray-200 shadow-sm">
              <div className="flex items-center mb-4">
                <FileText className="h-6 w-6 text-purple-500 mr-3" />
                <h3 className="text-lg font-semibold text-gray-900">Ready Templates</h3>
              </div>
              <p className="text-gray-600">
                Start with pre-built templates for React components, API calls, 
                data visualization, and more.
              </p>
            </div>

            <div className="bg-white p-6 rounded-lg border border-gray-200 shadow-sm">
              <div className="flex items-center mb-4">
                <Download className="h-6 w-6 text-indigo-500 mr-3" />
                <h3 className="text-lg font-semibold text-gray-900">Export & Import</h3>
              </div>
              <p className="text-gray-600">
                Download your code as files or upload existing files to continue 
                working on your projects.
              </p>
            </div>

            <div className="bg-white p-6 rounded-lg border border-gray-200 shadow-sm">
              <div className="flex items-center mb-4">
                <Monitor className="h-6 w-6 text-orange-500 mr-3" />
                <h3 className="text-lg font-semibold text-gray-900">Flexible Views</h3>
              </div>
              <p className="text-gray-600">
                Switch between split view, code-only, or output-only modes 
                based on your workflow preferences.
              </p>
            </div>

            <div className="bg-white p-6 rounded-lg border border-gray-200 shadow-sm">
              <div className="flex items-center mb-4">
                <Smartphone className="h-6 w-6 text-pink-500 mr-3" />
                <h3 className="text-lg font-semibold text-gray-900">Responsive Design</h3>
              </div>
              <p className="text-gray-600">
                Fully responsive interface that works seamlessly across 
                desktop, tablet, and mobile devices.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}