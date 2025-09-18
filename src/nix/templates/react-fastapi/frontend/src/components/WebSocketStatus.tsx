import { Wifi, WifiOff } from 'lucide-react';

interface WebSocketStatusProps {
  isConnected: boolean;
}

export function WebSocketStatus({ isConnected }: WebSocketStatusProps) {
  return (
    <div className="flex items-center space-x-2">
      {isConnected ? (
        <>
          <Wifi className="h-4 w-4 text-green-500" />
          <span className="text-sm text-green-500">Connected</span>
        </>
      ) : (
        <>
          <WifiOff className="h-4 w-4 text-red-500" />
          <span className="text-sm text-red-500">Disconnected</span>
        </>
      )}
    </div>
  );
}