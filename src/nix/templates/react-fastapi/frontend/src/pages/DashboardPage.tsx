import { useEffect, useState } from 'react';
import { LogOut, User } from 'lucide-react';
import { useAuthStore } from '@/stores/authStore';
import { useTaskStore } from '@/stores/taskStore';
import { useWebSocket } from '@/hooks/useWebSocket';
import type { WSMessage } from '@shared/types';
import { Button } from '@/components/Button';
import { TaskCard } from '@/components/TaskCard';
import { CreateTaskForm } from '@/components/CreateTaskForm';
import { WebSocketStatus } from '@/components/WebSocketStatus';

export default function DashboardPage() {
  const { user, logout } = useAuthStore();
  const { 
    tasks, 
    fetchTasks, 
    isLoading, 
    error,
    // Methods for WebSocket updates
    addTask,
    updateTaskInStore,
    removeTask,
  } = useTaskStore();
  
  const [wsMessages, setWsMessages] = useState<WSMessage[]>([]);

  const handleWebSocketMessage = (message: WSMessage) => {
    setWsMessages(prev => [message, ...prev.slice(0, 9)]); // Keep last 10 messages
    
    // Handle task updates from WebSocket
    switch (message.type) {
      case 'task_created':
        // Note: We don't add the task here because it would duplicate
        // the task that was already added when the user created it
        break;
      case 'task_updated':
        // Similarly, we don't update here to avoid conflicts
        break;
      case 'task_deleted':
        // We could remove the task here if it was deleted by another user
        break;
    }
  };

  const { isConnected } = useWebSocket({
    url: '/ws',
    onMessage: handleWebSocketMessage,
    onOpen: () => console.log('WebSocket connected'),
    onClose: () => console.log('WebSocket disconnected'),
    onError: (error) => console.error('WebSocket error:', error),
  });

  useEffect(() => {
    fetchTasks();
  }, [fetchTasks]);

  const completedTasks = tasks.filter(task => task.completed);
  const pendingTasks = tasks.filter(task => !task.completed);

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <header className="bg-white shadow-sm border-b">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-between h-16">
            <div className="flex items-center space-x-4">
              <h1 className="text-xl font-semibold text-gray-900">Task Manager</h1>
              <WebSocketStatus isConnected={isConnected} />
            </div>
            
            <div className="flex items-center space-x-4">
              <div className="flex items-center space-x-2">
                <User className="h-4 w-4" />
                <span className="text-sm text-gray-700">{user?.username}</span>
              </div>
              <Button variant="outline" size="sm" onClick={logout}>
                <LogOut className="h-4 w-4 mr-2" />
                Logout
              </Button>
            </div>
          </div>
        </div>
      </header>

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
          {/* Tasks Section */}
          <div className="lg:col-span-2 space-y-6">
            <div>
              <h2 className="text-2xl font-bold text-gray-900 mb-4">Your Tasks</h2>
              <CreateTaskForm />
            </div>

            {error && (
              <div className="bg-red-50 border border-red-200 text-red-600 px-4 py-3 rounded">
                {error}
              </div>
            )}

            {isLoading && tasks.length === 0 ? (
              <div className="text-center py-8">
                <div className="text-gray-500">Loading tasks...</div>
              </div>
            ) : (
              <>
                {/* Pending Tasks */}
                {pendingTasks.length > 0 && (
                  <div>
                    <h3 className="text-lg font-semibold text-gray-800 mb-3">
                      Pending ({pendingTasks.length})
                    </h3>
                    <div className="space-y-3">
                      {pendingTasks.map((task) => (
                        <TaskCard key={task.id} task={task} />
                      ))}
                    </div>
                  </div>
                )}

                {/* Completed Tasks */}
                {completedTasks.length > 0 && (
                  <div>
                    <h3 className="text-lg font-semibold text-gray-800 mb-3">
                      Completed ({completedTasks.length})
                    </h3>
                    <div className="space-y-3">
                      {completedTasks.map((task) => (
                        <TaskCard key={task.id} task={task} />
                      ))}
                    </div>
                  </div>
                )}

                {tasks.length === 0 && !isLoading && (
                  <div className="text-center py-8">
                    <div className="text-gray-500">No tasks yet. Create your first task!</div>
                  </div>
                )}
              </>
            )}
          </div>

          {/* Sidebar */}
          <div className="space-y-6">
            {/* Stats */}
            <div className="bg-white rounded-lg shadow p-6">
              <h3 className="text-lg font-semibold text-gray-800 mb-4">Statistics</h3>
              <div className="space-y-3">
                <div className="flex justify-between">
                  <span className="text-gray-600">Total Tasks:</span>
                  <span className="font-semibold">{tasks.length}</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-600">Pending:</span>
                  <span className="font-semibold text-orange-600">{pendingTasks.length}</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-gray-600">Completed:</span>
                  <span className="font-semibold text-green-600">{completedTasks.length}</span>
                </div>
                {tasks.length > 0 && (
                  <div className="flex justify-between">
                    <span className="text-gray-600">Progress:</span>
                    <span className="font-semibold">
                      {Math.round((completedTasks.length / tasks.length) * 100)}%
                    </span>
                  </div>
                )}
              </div>
            </div>

            {/* Real-time Updates */}
            <div className="bg-white rounded-lg shadow p-6">
              <h3 className="text-lg font-semibold text-gray-800 mb-4">Real-time Updates</h3>
              <div className="text-sm text-gray-600">
                {wsMessages.length === 0 ? (
                  <p>No recent updates</p>
                ) : (
                  <div className="space-y-2 max-h-40 overflow-y-auto">
                    {wsMessages.map((message, index) => (
                      <div key={index} className="p-2 bg-gray-50 rounded text-xs">
                        <div className="font-medium">{message.type}</div>
                        <div className="text-gray-500">
                          {JSON.stringify(message.data, null, 2)}
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}
