import { CodeWithOutputBlock } from '../../components/CodeWithOutputBlock'

export function CodeExamples() {
  return (
    <div className="space-y-8">
      <section>
        <h2 className="text-2xl font-bold text-gray-900 mb-4">Code Examples</h2>
        <p className="text-gray-600 mb-6">
          Interactive code examples showing how to use the API and implement common patterns.
        </p>

        <div className="space-y-8">
          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">JavaScript/TypeScript Example</h3>

            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`// Complete API client with authentication
const makeAuthenticatedRequest = async (url, options = {}) => {
  const token = localStorage.getItem('access_token');

  if (!token) {
    throw new Error('No authentication token found');
  }

  const response = await fetch(url, {
    ...options,
    headers: {
      'Authorization': \`Bearer \${token}\`,
      'Content-Type': 'application/json',
      ...options.headers
    }
  });

  if (response.status === 401) {
    localStorage.removeItem('access_token');
    throw new Error('Authentication failed');
  }

  return response;
};

// Login and create task
const loginAndCreateTask = async (username, password, taskTitle) => {
  // Login
  const formData = new FormData();
  formData.append('username', username);
  formData.append('password', password);

  const loginResponse = await fetch('http://localhost:8000/api/login', {
    method: 'POST',
    body: formData
  });

  const tokenData = await loginResponse.json();
  localStorage.setItem('access_token', tokenData.access_token);

  // Create task
  const taskResponse = await makeAuthenticatedRequest('http://localhost:8000/api/tasks', {
    method: 'POST',
    body: JSON.stringify({
      title: taskTitle,
      description: "Created via JavaScript client"
    })
  });

  return taskResponse.json();
};

// Usage
const task = await loginAndCreateTask("johndoe", "secret123", "JS Integration");
console.log(\`Task created: \${task.title} (ID: \${task.id})\`);`}
                language="javascript"
                filePath="examples/js_client.js"
                output={`Task created: JS Integration (ID: 42)`}
                isExampleOutput={true}
              />

              <CodeWithOutputBlock
                code={`// Example with error handling
const loginAndCreateTask = async (username, password, taskTitle) => {
  try {
    // Intentional error - wrong endpoint
    const loginResponse = await fetch('http://localhost:8000/api/wrong-login', {
      method: 'POST',
      body: formData
    });

    if (!loginResponse.ok) {
      throw new Error(\`Login failed: \${loginResponse.status}\`);
    }

    const tokenData = await loginResponse.json();
    localStorage.setItem('access_token', tokenData.access_token);

    return tokenData;
  } catch (error) {
    console.error('Authentication error:', error);
    throw error;
  }
};

// This will fail
const result = await loginAndCreateTask("johndoe", "wrongpass", "Test Task");`}
                language="javascript"
                filePath="examples/js_error_example.js"
                success={false}
                output={`Error: Login failed: 404
    at loginAndCreateTask (js_error_example.js:8:13)
    at async main (js_error_example.js:25:18)

Authentication error: Error: Login failed: 404
Fatal: Unable to authenticate user
Critical: Application cannot continue without valid credentials`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Python</h3>

            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`# Python API client using requests
import requests
import json
from typing import Dict, Any, Optional

class TaskAPIClient:
    def __init__(self, base_url: str = "http://localhost:8000"):
        self.base_url = base_url
        self.session = requests.Session()
        self.token: Optional[str] = None

    def login(self, username: str, password: str) -> Dict[str, Any]:
        """Authenticate user and store token"""
        data = {'username': username, 'password': password}
        response = self.session.post(f"{self.base_url}/api/login", data=data)
        response.raise_for_status()

        token_data = response.json()
        self.token = token_data['access_token']
        self.session.headers.update({
            'Authorization': f'Bearer {self.token}'
        })
        return token_data

    def create_task(self, title: str, description: str = "") -> Dict[str, Any]:
        """Create a new task"""
        task_data = {"title": title, "description": description}
        response = self.session.post(
            f"{self.base_url}/api/tasks",
            json=task_data
        )
        response.raise_for_status()
        return response.json()

# Example usage
if __name__ == "__main__":
    client = TaskAPIClient()

    # Login
    token_data = client.login("johndoe", "secret123")
    print(f"Logged in! Token expires in {token_data['expires_in']}s")

    # Create task
    task = client.create_task("Complete Python integration", "Add Python examples to docs")
    print(f"Created task: {task['title']} (ID: {task['id']})")`}
                language="python"
                filePath="examples/python_client.py"
                output={`Logged in! Token expires in 1800s
Created task: Complete Python integration (ID: 42)`}
                isExampleOutput={true}
              />

              <CodeWithOutputBlock
                code={`# Python error example - connection failure
import requests
from typing import Dict, Any

def test_connection_failure():
    """Demonstrate error handling with connection issues"""
    try:
        # Wrong port - will cause connection error
        response = requests.post(
            "http://localhost:9999/api/login",  # Wrong port
            data={'username': 'test', 'password': 'test'},
            timeout=5
        )
        response.raise_for_status()
        return response.json()

    except requests.exceptions.ConnectionError as e:
        print(f"Fatal: Connection failed - {e}")
        raise
    except requests.exceptions.Timeout as e:
        print(f"Error: Request timeout - {e}")
        raise
    except Exception as e:
        print(f"Critical: Unexpected error - {e}")
        raise

# This will fail
if __name__ == "__main__":
    try:
        result = test_connection_failure()
    except Exception as e:
        print(f"Application error: {e}")
        exit(1)`}
                language="python"
                filePath="examples/python_error.py"
                success={false}
                output={`Fatal: Connection failed - HTTPConnectionPool(host='localhost', port=9999): Max retries exceeded with url: /api/login (Caused by NewConnectionError('<urllib3.connection.HTTPConnection object at 0x7f8b8c0d4f40>: Failed to establish a new connection: [Errno 111] Connection refused'))

Critical: Unexpected error - HTTPConnectionPool(host='localhost', port=9999): Max retries exceeded with url: /api/login (Caused by NewConnectionError('<urllib3.connection.HTTPConnection object at 0x7f8b8c0d4f40>: Failed to establish a new connection: [Errno 111] Connection refused'))

Application error: HTTPConnectionPool(host='localhost', port=9999): Max retries exceeded with url: /api/login (Caused by NewConnectionError('<urllib3.connection.HTTPConnection object at 0x7f8b8c0d4f40>: Failed to establish a new connection: [Errno 111] Connection refused'))

Traceback (most recent call last):
  File "examples/python_error.py", line 28, in <module>
    result = test_connection_failure()
  File "examples/python_error.py", line 11, in <module>
    response = requests.post(
Warning: Connection pool exhausted, retrying...
Error: Max retries exceeded for localhost:9999`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">C++</h3>

            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`#include <iostream>
#include <string>
#include <curl/curl.h>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

class TaskAPIClient {
private:
    std::string base_url;
    std::string auth_token;
    CURL* curl;

    struct APIResponse {
        std::string data;
        long status_code;
    };

    static size_t WriteCallback(void* contents, size_t size, size_t nmemb, APIResponse* response) {
        size_t total_size = size * nmemb;
        response->data.append((char*)contents, total_size);
        return total_size;
    }

public:
    TaskAPIClient(const std::string& url = "http://localhost:8000") : base_url(url) {
        curl = curl_easy_init();
    }

    ~TaskAPIClient() {
        if (curl) curl_easy_cleanup(curl);
    }

    bool login(const std::string& username, const std::string& password) {
        if (!curl) return false;

        APIResponse response;
        std::string form_data = "username=" + username + "&password=" + password;

        curl_easy_setopt(curl, CURLOPT_URL, (base_url + "/api/login").c_str());
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, form_data.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

        CURLcode res = curl_easy_perform(curl);
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response.status_code);

        if (res == CURLE_OK && response.status_code == 200) {
            json token_data = json::parse(response.data);
            auth_token = token_data["access_token"];
            return true;
        }
        return false;
    }

    json create_task(const std::string& title, const std::string& description = "") {
        json task_data = {{"title", title}, {"description", description}};
        std::string json_string = task_data.dump();

        APIResponse response;
        struct curl_slist* headers = nullptr;
        std::string auth_header = "Authorization: Bearer " + auth_token;
        headers = curl_slist_append(headers, "Content-Type: application/json");
        headers = curl_slist_append(headers, auth_header.c_str());

        curl_easy_setopt(curl, CURLOPT_URL, (base_url + "/api/tasks").c_str());
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_string.c_str());
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

        curl_easy_perform(curl);
        curl_slist_free_all(headers);

        return json::parse(response.data);
    }
};`}
                language="cpp"
                filePath="examples/cpp_client.hpp"
                startLine={15}
                output={`Authentication successful!
Task created: {"id": 43, "title": "C++ integration complete"}`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Rust</h3>

            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`use reqwest;
use serde_json::{json, Value};
use std::collections::HashMap;
use tokio;

#[derive(Debug)]
pub struct TaskAPIClient {
    base_url: String,
    client: reqwest::Client,
    auth_token: Option<String>,
}

impl TaskAPIClient {
    pub fn new(base_url: &str) -> Self {
        Self {
            base_url: base_url.to_string(),
            client: reqwest::Client::new(),
            auth_token: None,
        }
    }

    pub async fn login(&mut self, username: &str, password: &str) -> Result<Value, Box<dyn std::error::Error>> {
        let mut form_data = HashMap::new();
        form_data.insert("username", username);
        form_data.insert("password", password);

        let response = self.client
            .post(&format!("{}/api/login", self.base_url))
            .form(&form_data)
            .send()
            .await?;

        if response.status().is_success() {
            let token_data: Value = response.json().await?;
            if let Some(token) = token_data["access_token"].as_str() {
                self.auth_token = Some(format!("Bearer {}", token));
            }
            Ok(token_data)
        } else {
            Err(format!("Login failed: {}", response.status()).into())
        }
    }

    pub async fn create_task(&self, title: &str, description: &str) -> Result<Value, Box<dyn std::error::Error>> {
        let task_data = json!({
            "title": title,
            "description": description
        });

        let mut request = self.client.post(&format!("{}/api/tasks", self.base_url));

        if let Some(ref token) = self.auth_token {
            request = request.header("Authorization", token);
        }

        let response = request
            .json(&task_data)
            .send()
            .await?;

        if response.status().is_success() {
            Ok(response.json().await?)
        } else {
            Err(format!("Task creation failed: {}", response.status()).into())
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut client = TaskAPIClient::new("http://localhost:8000");

    // Login
    let token_data = client.login("johndoe", "secret123").await?;
    println!("Login successful! Token type: {}", token_data["token_type"]);

    // Create task
    let task = client.create_task("Rust integration", "Add async Rust examples").await?;
    println!("Created task: {} (ID: {})", task["title"], task["id"]);

    Ok(())
}`}
                language="rust"
                filePath="examples/rust_client.rs"
                startLine={1}
                output={`Login successful! Token type: bearer
Created task: Rust integration (ID: 44)`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Haskell</h3>

            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import Data.Aeson
import Data.ByteString.Char8 qualified as C8
import Control.Monad.IO.Class (liftIO)

-- API data types with automatic JSON parsing
data LoginRequest = LoginRequest
  { username :: String
  , password :: String
  } deriving (Show)

data TaskRequest = TaskRequest
  { title :: String
  , description :: String
  } deriving (Show, Generic)

instance ToJSON TaskRequest
instance FromJSON TaskRequest

-- Monadic API client with error handling
type APIToken = String

loginUser :: String -> String -> IO (Either String APIToken)
loginUser user pass = do
  let formData = [("username", C8.pack user), ("password", C8.pack pass)]
  request <- parseRequest "POST http://localhost:8000/api/login"
  let request' = setRequestBodyURLEncoded formData request

  response <- httpJSON request'
  case getResponseStatusCode response of
    200 -> do
      let body = getResponseBody response :: Value
      case body ^? key "access_token" . _String of
        Just token -> return $ Right (toString token)
        Nothing -> return $ Left "Invalid response format"
    _ -> return $ Left "Authentication failed"

-- Pure function composition and higher-order functions
createTask :: APIToken -> String -> String -> IO (Either String Value)
createTask token title desc = do
  let taskData = TaskRequest title desc
  request <- parseRequest "POST http://localhost:8000/api/tasks"
  let request' = setRequestBodyJSON taskData
               $ addRequestHeader "Authorization" (C8.pack $ "Bearer " ++ token)
               $ request

  response <- httpJSON request'
  case getResponseStatusCode response of
    200 -> return $ Right (getResponseBody response)
    _ -> return $ Left "Task creation failed"

-- Functional pipeline with monadic composition
apiWorkflow :: String -> String -> String -> String -> IO ()
apiWorkflow user pass taskTitle taskDesc = do
  result <- loginUser user pass >>= \\case
    Left err -> return $ Left err
    Right token -> createTask token taskTitle taskDesc

  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right task -> putStrLn $ "Task created: " ++ show task

-- Type-safe computation with Maybe and Either
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

fibonacci :: Integer -> Integer
fibonacci n = fib n 0 1
  where
    fib 0 a _ = a
    fib n a b = fib (n-1) b (a+b)

main :: IO ()
main = apiWorkflow "johndoe" "secret123" "Haskell Integration" "Pure functional API client"`}
                language="haskell"
                filePath="examples/haskell_client.hs"
                startLine={1}
                output={`Task created: Object (fromList [("id",Number 47.0),("title",String "Haskell Integration"),("completed",Bool False)])`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Prolog</h3>

            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`% Prolog API client using HTTP library
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

% Task API client structure
:- dynamic(auth_token/1).

% Initialize client
init_client :-
    retractall(auth_token(_)).

% Login and store authentication token
login(Username, Password) :-
    BaseURL = 'http://localhost:8000',
    atom_concat(BaseURL, '/api/login', LoginURL),

    % Prepare form data
    FormData = [username=Username, password=Password],

    % Make HTTP POST request
    http_post(LoginURL, form_data(FormData), Response, []),

    % Parse JSON response
    atom_json_term(Response, TokenData, []),

    % Extract and store token
    get_dict(access_token, TokenData, Token),
    assertz(auth_token(Token)),

    format('Login successful! Token: ~w~n', [Token]).

% Create a new task
create_task(Title, Description, TaskId) :-
    auth_token(Token),
    BaseURL = 'http://localhost:8000',
    atom_concat(BaseURL, '/api/tasks', TaskURL),

    % Prepare JSON payload
    TaskData = _{title: Title, description: Description},
    atom_json_term(TaskJSON, TaskData, []),

    % Prepare headers with authentication
    atom_concat('Bearer ', Token, AuthHeader),
    Headers = ['Authorization'(AuthHeader), 'Content-Type'('application/json')],

    % Make authenticated HTTP POST request
    http_post(TaskURL, json(TaskData), Response, [request_header('Authorization'=AuthHeader)]),

    % Parse response
    atom_json_term(Response, ResponseData, []),
    get_dict(id, ResponseData, TaskId),
    get_dict(title, ResponseData, ResponseTitle),

    format('Task created: ~w (ID: ~w)~n', [ResponseTitle, TaskId]).

% Main execution predicate
run_example :-
    init_client,

    % Login
    login('johndoe', 'secret123'),

    % Create task
    create_task('Prolog integration', 'Add logic programming examples', TaskId),

    format('✓ Prolog logic programming demonstrated~n'),
    format('✓ Declarative HTTP client implementation~n'),
    format('✓ Pattern matching and unification~n').

% Query to run the example
?- run_example.`}
                language="prolog"
                filePath="examples/prolog_client.pl"
                startLine={25}
                output={`Login successful! Token: eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9...
Task created: Prolog integration (ID: 46)
✓ Prolog logic programming demonstrated
✓ Declarative HTTP client implementation
✓ Pattern matching and unification

?- true.`}
                isExampleOutput={true}
              />
            </div>
          </div>

          <div>
            <h3 className="text-xl font-semibold text-gray-900 mb-4">Error Handling Examples</h3>

            <div className="space-y-6">
              <CodeWithOutputBlock
                code={`// Comprehensive error handling wrapper
const apiRequest = async (url, options = {}) => {
  try {
    const response = await makeAuthenticatedRequest(url, options);

    // Handle different HTTP status codes
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));

      switch (response.status) {
        case 400:
          throw new Error(\`Bad Request: \${errorData.detail || 'Invalid data'}\`);
        case 401:
          throw new Error('Unauthorized: Please log in again');
        case 403:
          throw new Error('Forbidden: You don\'t have permission');
        case 404:
          throw new Error('Not Found: Resource doesn\'t exist');
        case 422:
          throw new Error(\`Validation Error: \${formatValidationErrors(errorData)}\`);
        case 500:
          throw new Error('Server Error: Please try again later');
        default:
          throw new Error(\`HTTP \${response.status}: \${errorData.detail || 'Unknown error'}\`);
      }
    }

    return response.json();
  } catch (error) {
    if (error.name === 'TypeError' && error.message.includes('fetch')) {
      throw new Error('Network Error: Please check your connection');
    }
    throw error;
  }
};

// Format validation errors for user-friendly display
const formatValidationErrors = (errorData) => {
  if (errorData.detail && Array.isArray(errorData.detail)) {
    return errorData.detail
      .map(err => \`\${err.loc.join('.')}: \${err.msg}\`)
      .join(', ');
  }
  return errorData.detail || 'Validation failed';
};

// Usage in React component with error boundaries
const TaskForm = () => {
  const [error, setError] = useState(null);
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (formData) => {
    setLoading(true);
    setError(null);

    try {
      await apiRequest('/api/tasks', {
        method: 'POST',
        body: JSON.stringify(formData)
      });
      // Success handling
    } catch (err) {
      setError(err.message);
    } finally {
      setLoading(false);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
      )}
      {/* Form fields */}
    </form>
  );
};`}
                language="tsx"
                filePath="src/utils/errorHandling.tsx"
                output={`// Testing different error scenarios:

// 1. Validation Error (422)
{
  "detail": [
    {
      "loc": ["body", "title"],
      "msg": "field required",
      "type": "value_error.missing"
    },
    {
      "loc": ["body", "email"],
      "msg": "field required",
      "type": "value_error.missing"
    }
  ]
}
→ "Validation Error: body.title: field required, body.email: field required"

// 2. Authentication Error (401)
→ "Unauthorized: Please log in again"
✓ User redirected to login page
✓ Access token cleared from localStorage

// 3. Network Error
→ "Network Error: Please check your connection"
✓ Retry mechanism triggered
✓ User notification displayed

// 4. Server Error (500)
→ "Server Error: Please try again later"
✓ Error logged to monitoring service
✓ Fallback UI displayed

✓ All error types handled gracefully
✓ User-friendly error messages shown
✓ Error boundaries prevent app crashes`}
              />
            </div>
          </div>
        </div>
      </section>
    </div>
  )
}