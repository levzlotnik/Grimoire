import { useState } from 'react';
import { Plus } from 'lucide-react';
import { useTaskStore } from '@/stores/taskStore';
import { Button } from './Button';
import { Input } from './Input';
import { Card, CardContent, CardHeader, CardTitle } from './Card';

export function CreateTaskForm() {
  const [title, setTitle] = useState('');
  const [description, setDescription] = useState('');
  const [isExpanded, setIsExpanded] = useState(false);
  
  const { createTask, isLoading } = useTaskStore();

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!title.trim()) return;

    try {
      await createTask({
        title: title.trim(),
        description: description.trim() || undefined,
      });
      
      setTitle('');
      setDescription('');
      setIsExpanded(false);
    } catch (error) {
      console.error('Failed to create task:', error);
    }
  };

  const handleCancel = () => {
    setTitle('');
    setDescription('');
    setIsExpanded(false);
  };

  if (!isExpanded) {
    return (
      <Card className="cursor-pointer hover:shadow-md transition-shadow" onClick={() => setIsExpanded(true)}>
        <CardContent className="flex items-center justify-center py-8">
          <div className="flex items-center space-x-2 text-muted-foreground">
            <Plus className="h-5 w-5" />
            <span>Add a new task</span>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-lg">Create New Task</CardTitle>
      </CardHeader>
      <CardContent>
        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <Input
              value={title}
              onChange={(e) => setTitle(e.target.value)}
              placeholder="Task title"
              required
              autoFocus
            />
          </div>
          
          <div>
            <Input
              value={description}
              onChange={(e) => setDescription(e.target.value)}
              placeholder="Task description (optional)"
            />
          </div>
          
          <div className="flex items-center space-x-2">
            <Button type="submit" disabled={!title.trim() || isLoading}>
              Create Task
            </Button>
            <Button type="button" variant="outline" onClick={handleCancel}>
              Cancel
            </Button>
          </div>
        </form>
      </CardContent>
    </Card>
  );
}