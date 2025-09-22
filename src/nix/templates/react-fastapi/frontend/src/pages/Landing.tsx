import { Link } from 'react-router-dom'
import { ChevronRight, Package, Rocket } from 'lucide-react'

export function Landing() {
  return (
    <div className="bg-white">
      {/* Hero Section */}
      <div className="relative isolate px-6 pt-14 lg:px-8">
        <div className="mx-auto max-w-2xl py-32 sm:py-48 lg:py-56">
          <div className="text-center">
            <h1 className="text-4xl font-bold tracking-tight text-gray-900 sm:text-6xl">
              React Showcase Template
            </h1>
            <p className="mt-6 text-lg leading-8 text-gray-600">
              A comprehensive React TypeScript template demonstrating modern web development patterns,
              data visualization, and interactive components.
            </p>
            <div className="mt-10 flex items-center justify-center gap-x-6">
              <Link
                to="/dashboard"
                className="rounded-md bg-indigo-600 px-3.5 py-2.5 text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600"
              >
                View Dashboard
              </Link>
              <Link
                to="/docs"
                className="text-sm font-semibold leading-6 text-gray-900 hover:text-gray-700"
              >
                Read docs <ChevronRight className="inline h-4 w-4" />
              </Link>
            </div>
          </div>
        </div>
      </div>

      {/* Features Section */}
      <div className="py-24 sm:py-32">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="mx-auto max-w-2xl lg:text-center">
            <h2 className="text-base font-semibold leading-7 text-indigo-600">Quick Start</h2>
            <p className="mt-2 text-3xl font-bold tracking-tight text-gray-900 sm:text-4xl">
              Everything you need to build modern React apps
            </p>
          </div>
          <div className="mx-auto mt-16 max-w-2xl sm:mt-20 lg:mt-24 lg:max-w-4xl">
            <dl className="grid max-w-xl grid-cols-1 gap-x-8 gap-y-10 lg:max-w-none lg:grid-cols-2 lg:gap-y-16">
              <div className="relative pl-16">
                <dt className="text-base font-semibold leading-7 text-gray-900">
                  <div className="absolute left-0 top-0 flex h-10 w-10 items-center justify-center rounded-lg bg-indigo-600">
                    <Package className="h-6 w-6 text-white" />
                  </div>
                  Modern Stack
                </dt>
                <dd className="mt-2 text-base leading-7 text-gray-600">
                  Built with React 19, TypeScript, Vite, and Tailwind CSS v4 for the best developer experience.
                </dd>
              </div>
              <div className="relative pl-16">
                <dt className="text-base font-semibold leading-7 text-gray-900">
                  <div className="absolute left-0 top-0 flex h-10 w-10 items-center justify-center rounded-lg bg-indigo-600">
                    <Rocket className="h-6 w-6 text-white" />
                  </div>
                  Fast Development
                </dt>
                <dd className="mt-2 text-base leading-7 text-gray-600">
                  Hot reload, instant feedback, and optimized build pipeline for rapid development cycles.
                </dd>
              </div>
            </dl>
          </div>
        </div>
      </div>

      {/* Installation Section */}
      <div className="bg-gray-50 py-24 sm:py-32">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="mx-auto max-w-2xl lg:text-center">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 sm:text-4xl">
              Installation
            </h2>
            <p className="mt-6 text-lg leading-8 text-gray-600">
              Get started with this template in minutes using Nix or traditional package managers.
            </p>
          </div>
          <div className="mx-auto mt-16 max-w-2xl">
            <div className="space-y-8">
              <div>
                <h3 className="text-lg font-semibold text-gray-900">Using Nix (Recommended)</h3>
                <div className="mt-4 rounded-lg bg-gray-900 p-4">
                  <code className="text-sm text-green-400">
                    nix develop<br />
                    npm run dev
                  </code>
                </div>
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900">Using npm</h3>
                <div className="mt-4 rounded-lg bg-gray-900 p-4">
                  <code className="text-sm text-green-400">
                    npm install<br />
                    npm run dev
                  </code>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}