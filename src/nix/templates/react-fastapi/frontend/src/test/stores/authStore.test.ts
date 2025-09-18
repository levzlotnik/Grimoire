import { describe, it, expect, beforeEach, vi } from 'vitest';
import { useAuthStore } from '@/stores/authStore';

// Mock the API
vi.mock('@/lib/api', () => ({
  authApi: {
    login: vi.fn(),
    register: vi.fn(),
    getCurrentUser: vi.fn(),
  },
}));

describe('AuthStore', () => {
  beforeEach(() => {
    // Reset store state before each test
    useAuthStore.setState({
      user: null,
      token: null,
      isAuthenticated: false,
      isLoading: false,
    });
    
    // Clear localStorage
    localStorage.clear();
  });

  it('initializes with default state', () => {
    const state = useAuthStore.getState();
    expect(state.user).toBeNull();
    expect(state.token).toBeNull();
    expect(state.isAuthenticated).toBeFalsy();
    expect(state.isLoading).toBeFalsy();
  });

  it('sets loading state during login', () => {
    const state = useAuthStore.getState();
    
    // Mock a pending login
    state.login('testuser', 'password');
    
    const newState = useAuthStore.getState();
    expect(newState.isLoading).toBeTruthy();
  });

  it('clears user data on logout', () => {
    // Set some initial state
    useAuthStore.setState({
      user: { id: 1, username: 'test', email: 'test@example.com', is_active: true, created_at: '2023-01-01' },
      token: 'fake-token',
      isAuthenticated: true,
    });

    const state = useAuthStore.getState();
    state.logout();

    const newState = useAuthStore.getState();
    expect(newState.user).toBeNull();
    expect(newState.token).toBeNull();
    expect(newState.isAuthenticated).toBeFalsy();
  });
});