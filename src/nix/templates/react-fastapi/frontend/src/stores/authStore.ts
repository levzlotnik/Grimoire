import { create } from 'zustand';
import { persist } from 'zustand/middleware';
import type { User } from '@shared/types';
import { authApi } from '@/lib/api';

interface AuthState {
  user: User | null;
  token: string | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  login: (username: string, password: string) => Promise<void>;
  register: (username: string, email: string, password: string) => Promise<void>;
  logout: () => void;
  getCurrentUser: () => Promise<void>;
}

export const useAuthStore = create<AuthState>()(
  persist(
    (set, get) => ({
      user: null,
      token: null,
      isAuthenticated: false,
      isLoading: false,

      login: async (username: string, password: string) => {
        set({ isLoading: true });
        try {
          const response = await authApi.login({ username, password });
          const { access_token } = response.data;
          
          localStorage.setItem('token', access_token);
          set({ token: access_token, isAuthenticated: true });
          
          // Get user info after login
          await get().getCurrentUser();
        } catch (error) {
          console.error('Login failed:', error);
          throw error;
        } finally {
          set({ isLoading: false });
        }
      },

      register: async (username: string, email: string, password: string) => {
        set({ isLoading: true });
        try {
          const response = await authApi.register({ username, email, password });
          const user = response.data;
          
          // Auto-login after successful registration
          await get().login(username, password);
        } catch (error) {
          console.error('Registration failed:', error);
          throw error;
        } finally {
          set({ isLoading: false });
        }
      },

      logout: () => {
        localStorage.removeItem('token');
        set({
          user: null,
          token: null,
          isAuthenticated: false,
        });
      },

      getCurrentUser: async () => {
        try {
          const response = await authApi.getCurrentUser();
          set({ user: response.data });
        } catch (error) {
          console.error('Failed to get current user:', error);
          get().logout();
        }
      },
    }),
    {
      name: 'auth-storage',
      partialize: (state) => ({ 
        token: state.token, 
        isAuthenticated: state.isAuthenticated 
      }),
    }
  )
);