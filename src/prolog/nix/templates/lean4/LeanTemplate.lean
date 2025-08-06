-- Simple Lean4 template demonstrating basic functionality
def helloWorld : String := "Hello from the library!"

def main : IO Unit := do
  IO.println "Hello, World!"
  IO.println helloWorld
