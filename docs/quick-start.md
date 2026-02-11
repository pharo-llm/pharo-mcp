## Quick Start

### 1. Create a Server

```smalltalk
server := McpServer name: 'my-pharo-mcp' version: '1.0.0'.
```

### 2. Register Tools

```smalltalk
server addTool: (McpTool
  name: 'evaluate'
  description: 'Evaluate a Pharo expression and return the result'
  inputSchema: (McpSchema
    object: { 'expression' -> (McpSchema string: 'Pharo expression to evaluate') }
    required: #( 'expression' ))
  handler: [ :args |
    | result |
    result := Compiler evaluate: (args at: 'expression').
    McpToolResult successText: result printString ]).
```

### 3. Register Resources

```smalltalk
server addResource: (McpResource
  uri: 'pharo://system/version'
  name: 'System Version'
  description: 'Current Pharo system version'
  mimeType: 'text/plain'
  handler: [ { McpContentItem text: SystemVersion current versionString } ]).
```

### 4. Register Prompts

```smalltalk
server addPrompt: (McpPrompt
  name: 'review-class'
  description: 'Generate a code review for a Pharo class'
  arguments: { McpPromptArgument required: 'className' description: 'Name of the class to review' }
  handler: [ :args |
    Dictionary new
      at: 'description' put: 'Code review for ' , (args at: 'className');
      at: 'messages' put: {
        (McpPromptMessage user: 'Please review the class ' , (args at: 'className') , ' in this Pharo image.') asDictionary };
      yourself ]).
```

### 5. Connect Transport and Start

```smalltalk
transport := McpStdioTransport stdin: Stdio stdin stdout: Stdio stdout.
server transport: transport.
server start.
```
