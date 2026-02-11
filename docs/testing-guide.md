# Testing Guide

LLM-Pharo-MCP has a comprehensive test suite in the `LLM-Pharo-MCP-Tests` package. This guide explains the testing approach and how to write tests for your own MCP extensions.

## Running Tests

### In a Pharo Image

```smalltalk
"Run all MCP tests"
(TestSuite forPackage: 'LLM-Pharo-MCP-Tests') run.

"Run a specific test class"
McpServerTest suite run.

"Run a single test"
(McpServerTest selector: #testHandlePing) run.
```

### From Command Line

```bash
smalltalkci -s Pharo64-14
```

## Test Suite Overview

| Test Class | Tests | What It Covers |
|------------|-------|----------------|
| `McpConstantsTest` | 8 | Protocol version, server defaults, error codes |
| `McpJsonRpcErrorTest` | 11 | Error creation, predefined errors, serialization |
| `McpJsonRpcRequestTest` | 5 | Request creation, serialization, JSON roundtrip |
| `McpJsonRpcResponseTest` | 6 | Success/error responses, serialization |
| `McpJsonRpcNotificationTest` | 6 | Notification creation, serialization |
| `McpJsonRpcMessageTest` | 10 | Message parsing (request/response/notification/error), roundtrips |
| `McpSchemaTest` | 13 | All schema types, nesting, required fields |
| `McpContentItemTest` | 6 | Text, image, resource content types and serialization |
| `McpToolResultTest` | 6 | Success/error results, multiple content items |
| `McpToolTest` | 5 | Tool creation, execution, error handling, serialization |
| `McpResourceTest` | 5 | Resource creation, reading, error handling, serialization |
| `McpResourceTemplateTest` | 9 | URI matching, parameter extraction, reading, serialization |
| `McpPromptArgumentTest` | 5 | Optional/required arguments, serialization |
| `McpPromptTest` | 6 | Prompt creation, execution, required args validation |
| `McpPromptMessageTest` | 4 | User/assistant messages, serialization |
| `McpServerCapabilitiesTest` | 7 | Default values, toggling capabilities, serialization |
| `McpInMemoryTransportTest` | 8 | Lifecycle, sending, receiving, reset |
| `McpServerTest` | 35+ | Full integration: init, ping, tools, resources, prompts, errors |

## Testing with McpInMemoryTransport

The key to testing MCP is the `McpInMemoryTransport`, which allows you to:

1. Simulate client messages with `simulateReceive:`
2. Inspect server responses with `lastSentParsed`
3. Check all messages with `sentMessages`

### Basic Test Setup

```smalltalk
TestCase subclass: #MyMcpTest
  instanceVariableNames: 'server transport'
  classVariableNames: ''
  package: 'My-Tests'.

MyMcpTest >> setUp [
  server := McpServer name: 'test' version: '1.0'.
  transport := McpInMemoryTransport new.
  server transport: transport.
  server start.
  "Register your tools, resources, prompts here"
]

MyMcpTest >> tearDown [
  server stop
]
```

### Testing a Tool

```smalltalk
MyMcpTest >> testMyTool [
  | response result |

  "Register the tool"
  server addTool: (McpTool
    name: 'add'
    description: 'Add two numbers'
    inputSchema: (McpSchema
      object: { 'a' -> McpSchema number. 'b' -> McpSchema number }
      required: #( 'a' 'b' ))
    handler: [ :args |
      McpToolResult successText: ((args at: 'a') + (args at: 'b')) printString ]).

  "Simulate the client calling the tool"
  transport simulateReceive: (McpJsonRpcRequest
    id: 1
    method: 'tools/call'
    params: (Dictionary new
      at: 'name' put: 'add';
      at: 'arguments' put: (Dictionary new at: 'a' put: 3; at: 'b' put: 4; yourself);
      yourself)) asJsonString.

  "Verify the response"
  response := transport lastSentParsed.
  result := response at: 'result'.
  self assert: ((result at: 'content') first at: 'text') equals: '7'.
  self deny: (result includesKey: 'isError')
]
```

### Testing a Resource

```smalltalk
MyMcpTest >> testMyResource [
  | response contents |

  server addResource: (McpResource
    uri: 'test://data'
    name: 'Test Data'
    description: 'Test data resource'
    handler: [ { McpContentItem text: 'test-content' } ]).

  transport simulateReceive: (McpJsonRpcRequest
    id: 1
    method: 'resources/read'
    params: (Dictionary new at: 'uri' put: 'test://data'; yourself)) asJsonString.

  response := transport lastSentParsed.
  contents := (response at: 'result') at: 'contents'.
  self assert: contents size equals: 1.
  self assert: (contents first at: 'text') equals: 'test-content'
]
```

### Testing Error Handling

```smalltalk
MyMcpTest >> testToolError [
  | response result |

  server addTool: (McpTool
    name: 'fail'
    description: 'Always fails'
    handler: [ :args | self error: 'Intentional failure' ]).

  transport simulateReceive: (McpJsonRpcRequest
    id: 1
    method: 'tools/call'
    params: (Dictionary new
      at: 'name' put: 'fail';
      at: 'arguments' put: Dictionary new;
      yourself)) asJsonString.

  response := transport lastSentParsed.
  result := response at: 'result'.
  self assert: (result at: 'isError') equals: true
]
```

### Testing Protocol Errors

```smalltalk
MyMcpTest >> testUnknownMethod [
  | response |

  transport simulateReceive: (McpJsonRpcRequest
    id: 1
    method: 'unknown/method') asJsonString.

  response := transport lastSentParsed.
  self assert: (response includesKey: 'error').
  self assert: ((response at: 'error') at: 'code') equals: -32601
]
```

## Helper Methods

The `McpServerTest` class demonstrates a useful helper pattern:

```smalltalk
"Send a request and get the parsed response"
sendRequest: method id: anId [
  | request |
  request := McpJsonRpcRequest id: anId method: method.
  transport simulateReceive: request asJsonString.
  ^ transport lastSentParsed
]

sendRequest: method id: anId params: params [
  | request |
  request := McpJsonRpcRequest id: anId method: method params: params.
  transport simulateReceive: request asJsonString.
  ^ transport lastSentParsed
]
```

## Testing Individual Components

You can also test MCP components in isolation without a server:

```smalltalk
"Test a schema"
schema := McpSchema object: { 'name' -> McpSchema string } required: #( 'name' ).
dict := schema asDictionary.
self assert: (dict at: 'type') equals: 'object'.

"Test a tool result"
result := McpToolResult successText: 'OK'.
self deny: result isError.
self assert: result content first text equals: 'OK'.

"Test JSON-RPC parsing"
msg := McpJsonRpcMessage fromJsonString: '{"jsonrpc":"2.0","id":1,"method":"ping"}'.
self assert: (msg isKindOf: McpJsonRpcRequest).
self assert: msg method equals: 'ping'.
```
