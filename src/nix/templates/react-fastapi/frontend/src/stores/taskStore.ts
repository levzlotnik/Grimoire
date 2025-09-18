import { create } from 'zustand';
import type { Task, TaskCreate, TaskUpdate } from '@shared/types';
import { tasksApi } from '@/lib/api';

interface TaskState {
  tasks: Task[];
  isLoading: boolean;
  error: string | null;
  fetchTasks: () => Promise<void>;
  createTask: (task: TaskCreate) => Promise<void>;
  updateTask: (id: number, task: TaskUpdate) => Promise<void>;
  deleteTask: (id: number) => Promise<void>;
  toggleTask: (id: number) => Promise<void>;
  addTask: (task: Task) => void;
  updateTaskInStore: (task: Task) => void;
  removeTask: (id: number) => void;
}

export const useTaskStore = create<TaskState>()((set, get) => ({
  tasks: [],
  isLoading: false,
  error: null,

  fetchTasks: async () => {
    set({ isLoading: true, error: null });
    try {
      const response = await tasksApi.getAllTasks();
      set({ tasks: response.data });
    } catch (error: any) {
      set({ error: error.response?.data?.detail || 'Failed to fetch tasks' });
    } finally {
      set({ isLoading: false });
    }
  },

  createTask: async (taskData: TaskCreate) => {
    set({ isLoading: true, error: null });
    try {
      const response = await tasksApi.createTask(taskData);
      set(state => ({ 
        tasks: [...state.tasks, response.data] 
      }));
    } catch (error: any) {
      set({ error: error.response?.data?.detail || 'Failed to create task' });
      throw error;
    } finally {
      set({ isLoading: false });
    }
  },

  updateTask: async (id: number, taskData: TaskUpdate) => {
    set({ isLoading: true, error: null });
    try {
      const response = await tasksApi.updateTask(id, taskData);
      set(state => ({
        tasks: state.tasks.map(task => 
          task.id === id ? response.data : task
        )
      }));
    } catch (error: any) {
      set({ error: error.response?.data?.detail || 'Failed to update task' });
      throw error;
    } finally {
      set({ isLoading: false });
    }
  },

  deleteTask: async (id: number) => {
    set({ isLoading: true, error: null });
    try {
      await tasksApi.deleteTask(id);
      set(state => ({
        tasks: state.tasks.filter(task => task.id !== id)
      }));
    } catch (error: any) {
      set({ error: error.response?.data?.detail || 'Failed to delete task' });
      throw error;
    } finally {
      set({ isLoading: false });
    }
  },

  toggleTask: async (id: number) => {
    const task = get().tasks.find(t => t.id === id);
    if (task) {
      await get().updateTask(id, { completed: !task.completed });
    }
  },

  // Methods for WebSocket updates
  addTask: (task: Task) => {
    set(state => ({ 
      tasks: [...state.tasks, task] 
    }));
  },

  updateTaskInStore: (updatedTask: Task) => {
    set(state => ({
      tasks: state.tasks.map(task => 
        task.id === updatedTask.id ? updatedTask : task
      )
    }));
  },

  removeTask: (id: number) => {
    set(state => ({
      tasks: state.tasks.filter(task => task.id !== id)
    }));
  },
}));