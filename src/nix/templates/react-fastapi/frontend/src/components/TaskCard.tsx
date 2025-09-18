import { useState } from 'react';
import { Trash2, Edit3, Check, X } from 'lucide-react';
import type { Task } from '@shared/types';
import { useTaskStore } from '@/stores/taskStore';
import { formatDate } from '@/lib/utils';
import { Button } from './Button';
import { Input } from './Input';
import { Card, CardContent, CardHeader, CardTitle } from './Card';

interface TaskCardProps {
  task: Task;
}

export function TaskCard({ task }: TaskCardProps) {
  const [isEditing, setIsEditing] = useState(false);
  const [editTitle, setEditTitle] = useState(task.title);
  const [editDescription, setEditDescription] = useState(task.description || '');
  
  const { toggleTask, updateTask, deleteTask, isLoading } = useTaskStore();

  const handleToggleComplete = () => {
    toggleTask(task.id);
  };

  const handleEdit = () => {
    setIsEditing(true);
  };

  const handleSaveEdit = async () => {
    try {
      await updateTask(task.id, {
        title: editTitle,
        description: editDescription || undefined,
      });
      setIsEditing(false);
    } catch (error) {
      console.error('Failed to update task:', error);
    }
  };

  const handleCancelEdit = () => {
    setEditTitle(task.title);
    setEditDescription(task.description || '');
    setIsEditing(false);
  };

  const handleDelete = async () => {
    if (confirm('Are you sure you want to delete this task?')) {
      try {
        await deleteTask(task.id);
      } catch (error) {
        console.error('Failed to delete task:', error);
      }
    }
  };

  return (
    <Card className={`transition-opacity ${task.completed ? 'opacity-60' : ''}`}>
      <CardHeader className="pb-3">
        <div className="flex items-start justify-between">
          {isEditing ? (
            <div className="flex-1 space-y-2">
              <Input
                value={editTitle}
                onChange={(e) => setEditTitle(e.target.value)}
                placeholder="Task title"
                className="font-semibold"
              />
              <Input
                value={editDescription}
                onChange={(e) => setEditDescription(e.target.value)}
                placeholder="Task description (optional)"
              />
            </div>
          ) : (
            <div className="flex-1">
              <CardTitle className={`text-lg ${task.completed ? 'line-through' : ''}`}>
                {task.title}
              </CardTitle>
              {task.description && (
                <p className={`text-sm text-muted-foreground mt-1 ${task.completed ? 'line-through' : ''}`}>
                  {task.description}
                </p>
              )}
            </div>
          )}
          
          <div className="flex items-center space-x-2 ml-4">
            {isEditing ? (
              <>
                <Button size="icon" variant="outline" onClick={handleSaveEdit} disabled={isLoading}>
                  <Check className="h-4 w-4" />
                </Button>
                <Button size="icon" variant="outline" onClick={handleCancelEdit}>
                  <X className="h-4 w-4" />
                </Button>
              </>
            ) : (
              <>
                <Button size="icon" variant="outline" onClick={handleEdit}>
                  <Edit3 className="h-4 w-4" />
                </Button>
                <Button size="icon" variant="destructive" onClick={handleDelete} disabled={isLoading}>
                  <Trash2 className="h-4 w-4" />
                </Button>
              </>
            )}
          </div>
        </div>
      </CardHeader>
      
      <CardContent className="pt-0">
        <div className="flex items-center justify-between">
          <label className="flex items-center space-x-2 cursor-pointer">
            <input
              type="checkbox"
              checked={task.completed}
              onChange={handleToggleComplete}
              className="w-4 h-4 text-primary border-gray-300 rounded focus:ring-primary"
              disabled={isLoading}
            />
            <span className="text-sm">
              {task.completed ? 'Completed' : 'Mark as complete'}
            </span>
          </label>
          
          <div className="text-xs text-muted-foreground">
            <div>Created: {formatDate(task.created_at)}</div>
            {task.updated_at !== task.created_at && (
              <div>Updated: {formatDate(task.updated_at)}</div>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}