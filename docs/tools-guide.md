# Tools Guide

Tools are the primary way an LLM interacts with your Pharo environment through MCP. A tool is a callable operation with a name, description, input schema, and a handler block.

## Creating a Tool

### Basic Tool (No Parameters)

```smalltalk
tool := McpTool
  name: 'system-info'
  description: 'Get information about the running Pharo system'
  handler: [ :args |
    McpToolResult successText: SystemVersion current versionString ].
```

### Tool with Parameters

```smalltalk
tool := McpTool
  name: 'evaluate'
  description: 'Evaluate a Pharo expression and return the result'
  inputSchema: (McpSchema
    object: { 'expression' -> (McpSchema string: 'The Pharo expression to evaluate') }
    required: #( 'expression' ))
  handler: [ :args |
    | result |
    result := Compiler evaluate: (args at: 'expression').
    McpToolResult successText: result printString ].
```

### Tool with Multiple Parameters

```smalltalk
tool := McpTool
  name: 'search-methods'
  description: 'Search for methods matching a pattern in a class'
  inputSchema: (McpSchema
    object: {
      'className' -> (McpSchema string: 'The class to search in').
      'pattern' -> (McpSchema string: 'Search pattern for method selectors').
      'includeSuper' -> (McpSchema boolean: 'Include superclass methods') }
    required: #( 'className' 'pattern' ))
  handler: [ :args |
    | cls pattern methods |
    cls := Smalltalk globals at: (args at: 'className') asSymbol ifAbsent: [ nil ].
    cls ifNil: [ ^ McpToolResult errorText: 'Class not found' ].
    pattern := args at: 'pattern'.
    methods := cls selectors select: [ :sel | sel includesSubstring: pattern ].
    McpToolResult successText: (', ' join: methods) ].
```

## Registering Tools

```smalltalk
server := McpServer default.
server addTool: tool.
```

You can register multiple tools:

```smalltalk
server addTool: evaluateTool.
server addTool: inspectTool.
server addTool: searchTool.
```

## Removing Tools

```smalltalk
server removeTool: 'evaluate'.
```

## Input Schema Types

The `McpSchema` class supports all JSON Schema types:

```smalltalk
"Primitives"
McpSchema string.
McpSchema string: 'A description'.
McpSchema number.
McpSchema integer.
McpSchema boolean.

"Object with properties"
McpSchema object: {
  'name' -> McpSchema string.
  'age' -> McpSchema integer }.

"Object with required fields"
McpSchema
  object: { 'name' -> McpSchema string. 'email' -> McpSchema string }
  required: #( 'name' 'email' ).

"Array of items"
McpSchema array: McpSchema string.

"Enum (string with allowed values)"
McpSchema enum: #( 'debug' 'info' 'warning' 'error' ).

"Nested objects"
McpSchema object: {
  'person' -> (McpSchema object: {
    'name' -> McpSchema string.
    'address' -> (McpSchema object: {
      'city' -> McpSchema string }) }) }.
```

## Tool Results

### Success Result

```smalltalk
"Simple text result"
McpToolResult successText: 'Operation completed'.

"Multiple content items"
McpToolResult success: {
  McpContentItem text: 'Found 3 results:'.
  McpContentItem text: '1. Object'.
  McpContentItem text: '2. String'.
  McpContentItem text: '3. Array' }.

"Image result"
McpToolResult success: {
  McpContentItem image: base64EncodedData mimeType: 'image/png' }.
```

### Error Result

```smalltalk
"Simple error"
McpToolResult errorText: 'Class not found: FooBar'.

"Error with details"
McpToolResult error: {
  McpContentItem text: 'Evaluation failed'.
  McpContentItem text: 'Error: MessageNotUnderstood: UndefinedObject>>foo' }.
```

### Automatic Error Handling

If your handler block raises an exception, it is automatically caught and returned as an error result:

```smalltalk
tool := McpTool
  name: 'risky'
  description: 'Might fail'
  handler: [ :args |
    "If this raises an error, it becomes McpToolResult errorText: ex messageText"
    (Compiler evaluate: (args at: 'code')).
    McpToolResult successText: 'Done' ].
```

## Example: Pharo Introspection Tools

```smalltalk
"List all classes in a package"
server addTool: (McpTool
  name: 'list-classes'
  description: 'List all classes in a Pharo package'
  inputSchema: (McpSchema
    object: { 'package' -> (McpSchema string: 'Package name') }
    required: #( 'package' ))
  handler: [ :args |
    | pkg classes |
    pkg := RPackageOrganizer default
      packageNamed: (args at: 'package')
      ifAbsent: [ ^ McpToolResult errorText: 'Package not found' ].
    classes := pkg definedClasses collect: #name.
    McpToolResult successText: (String lf join: classes sorted) ]).

"Get method source code"
server addTool: (McpTool
  name: 'method-source'
  description: 'Get the source code of a method'
  inputSchema: (McpSchema
    object: {
      'className' -> (McpSchema string: 'Class name').
      'selector' -> (McpSchema string: 'Method selector') }
    required: #( 'className' 'selector' ))
  handler: [ :args |
    | cls method |
    cls := Smalltalk globals at: (args at: 'className') asSymbol ifAbsent: [ nil ].
    cls ifNil: [ ^ McpToolResult errorText: 'Class not found' ].
    method := cls >> (args at: 'selector') asSymbol.
    McpToolResult successText: method sourceCode ]).
```
