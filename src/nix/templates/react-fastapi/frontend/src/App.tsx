import { BrowserRouter as Router, Routes, Route } from 'react-router-dom'
import { Navigation } from './components/Navigation'
import { Landing } from './pages/Landing'
import { Dashboard } from './pages/Dashboard'
import { Documentation } from './pages/Documentation'
import { Showcase } from './pages/Showcase'

function App() {
  return (
    <Router>
      <div className="min-h-screen bg-white">
        <Navigation />
        <main>
          <Routes>
            <Route path="/" element={<Landing />} />
            <Route path="/dashboard" element={<Dashboard />} />
            <Route path="/docs" element={<Documentation />} />
            <Route path="/showcase" element={<Showcase />} />
          </Routes>
        </main>
      </div>
    </Router>
  )
}

export default App
