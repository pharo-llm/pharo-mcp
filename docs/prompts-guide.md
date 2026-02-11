# Prompts Guide

Prompts are pre-defined templates that provide structured interactions between the LLM and the Pharo environment. They allow you to create reusable conversation patterns.

## Creating a Prompt

### Simple Prompt (No Arguments)

```smalltalk
prompt := McpPrompt
  name: 'system-overview'
  description: 'Generate an overview of the Pharo system'
  handler: [ :args |
    Dictionary new
      at: 'description' put: 'Pharo system overview';
      at: 'messages' put: {
        (McpPromptMessage user: 'Describe the current state of this Pharo system, including loaded packages and key classes.') asDictionary };
      yourself ].
```

### Prompt with Arguments

```smalltalk
prompt := McpPrompt
  name: 'review-class'
  description: 'Review a Pharo class for code quality'
  arguments: {
    McpPromptArgument required: 'className' description: 'The name of the class to review'.
    McpPromptArgument name: 'focus' description: 'Specific area to focus on (design, performance, naming)' }
  handler: [ :args |
    | className focus msg |
    className := args at: 'className'.
    focus := args at: 'focus' ifAbsent: [ 'general' ].
    msg := 'Please review the Pharo class "' , className , '". Focus on: ' , focus , '.'.
    Dictionary new
      at: 'description' put: 'Code review for ' , className;
      at: 'messages' put: {
        (McpPromptMessage user: msg) asDictionary };
      yourself ].
```

## Prompt Arguments

### Required Arguments

Required arguments must be provided when the prompt is invoked:

```smalltalk
McpPromptArgument required: 'className' description: 'The class to analyze'.
```

### Optional Arguments

Optional arguments may be omitted:

```smalltalk
McpPromptArgument name: 'format' description: 'Output format (text, json, markdown)'.
```

### Explicit Required Flag

```smalltalk
McpPromptArgument name: 'depth' description: 'Analysis depth' required: true.
McpPromptArgument name: 'style' description: 'Output style' required: false.
```

## Prompt Handler Return Format

The handler block receives a dictionary of arguments and must return a dictionary with:

- `'description'` (String): A description of this prompt instance
- `'messages'` (Array): An array of message dictionaries

Each message dictionary has:
- `'role'` (String): `'user'` or `'assistant'`
- `'content'` (Dictionary): A content item dictionary

### Using McpPromptMessage

The `McpPromptMessage` class provides convenience constructors:

```smalltalk
"User message with text"
McpPromptMessage user: 'Please analyze this code.'
"Assistant message with text"
McpPromptMessage assistant: 'I will analyze the code for potential issues.'
"Custom role with image content"
McpPromptMessage
  role: 'user'
  content: (McpContentItem image: base64Data mimeType: 'image/png')
```

## Multi-Turn Prompts

Prompts can define multi-turn conversations:

```smalltalk
prompt := McpPrompt
  name: 'refactor-method'
  description: 'Guide through refactoring a method'
  arguments: {
    McpPromptArgument required: 'className' description: 'The class'.
    McpPromptArgument required: 'selector' description: 'The method selector' }
  handler: [ :args |
    | className selector |
    className := args at: 'className'.
    selector := args at: 'selector'.
    Dictionary new
      at: 'description' put: 'Refactoring ' , className , '>>' , selector;
      at: 'messages' put: {
        (McpPromptMessage user: 'I need to refactor the method ' , className , '>>' , selector , '. Please analyze it and suggest improvements.') asDictionary.
        (McpPromptMessage assistant: 'I will analyze the method and provide refactoring suggestions. Let me first examine the current implementation.') asDictionary.
        (McpPromptMessage user: 'Please consider readability, performance, and adherence to Smalltalk conventions.') asDictionary };
      yourself ].
```

## Registering and Managing Prompts

```smalltalk
"Register"
server addPrompt: prompt.
"Look up"
prompt := server promptNamed: 'review-class'.
"Remove"
server removePrompt: 'review-class'.
```

## Validation

The server validates required arguments when `prompts/get` is called. If a required argument is missing, the handler will raise an error that is propagated back to the client.

## Example: Pharo-Specific Prompts

```smalltalk
"Code documentation generator"
server addPrompt: (McpPrompt
  name: 'document-class'
  description: 'Generate documentation for a Pharo class'
  arguments: {
    McpPromptArgument required: 'className' description: 'Class to document'.
    McpPromptArgument name: 'style' description: 'Documentation style (brief, detailed)' }
  handler: [ :args |
    | className style |
    className := args at: 'className'.
    style := args at: 'style' ifAbsent: [ 'detailed' ].
    Dictionary new
      at: 'description' put: 'Documentation for ' , className;
      at: 'messages' put: {
        (McpPromptMessage user:
          'Generate ' , style , ' documentation for the Pharo class "' , className ,
          '". Include class purpose, key methods, usage examples, and relationships with other classes.') asDictionary };
      yourself ]).
"Test generation"
server addPrompt: (McpPrompt
  name: 'generate-tests'
  description: 'Generate unit tests for a Pharo class'
  arguments: {
    McpPromptArgument required: 'className' description: 'Class to test' }
  handler: [ :args |
    | className |
    className := args at: 'className'.
    Dictionary new
      at: 'description' put: 'Test generation for ' , className;
      at: 'messages' put: {
        (McpPromptMessage user:
          'Generate comprehensive unit tests for the Pharo class "' , className ,
          '". Cover all public methods, edge cases, and error conditions. Use the SUnit framework.') asDictionary };
      yourself ]).
```