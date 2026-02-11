# Architecture

## Overview

LLM-Pharo-MCP follows a layered architecture that cleanly separates protocol handling, message serialization, and domain logic.

```
+-----------------------------------------------------+
|                    AI Client (LLM)                   |
+-----------------------------------------------------+
                         |
                    JSON-RPC 2.0
                         |
+-----------------------------------------------------+
|                   Transport Layer                    |
|         (McpStdioTransport / McpInMemoryTransport)   |
+-----------------------------------------------------+
                         |
+-----------------------------------------------------+
|                Message Parsing Layer                 |
|  McpJsonRpcMessage / Request / Response / Notification|
+-----------------------------------------------------+
                         |
+-----------------------------------------------------+
|                    McpServer                         |
|          (Routing + Request Dispatching)             |
+-----------------------------------------------------+
          |              |              |
+---------+--+  +--------+---+  +------+--------+
|   Tools    |  | Resources  |  |   Prompts     |
|  McpTool   |  | McpResource|  |  McpPrompt    |
|            |  | McpResource|  |  McpPromptArg |
|            |  |  Template  |  |  McpPromptMsg |
+------------+  +------------+  +---------------+
```

## Layer Descriptions

### Transport Layer

Responsible for reading and writing raw JSON strings. Two implementations:

- **`McpStdioTransport`**: Production transport using stdin/stdout. Reads lines from stdin and writes JSON responses to stdout. Used when running the MCP server as a subprocess.
- **`McpInMemoryTransport`**: Test transport that stores sent messages in memory and allows simulating received messages. Used extensively in the test suite.

Both transports implement a common interface:
- `start` / `stop` -- Lifecycle management
- `sendMessage:` -- Send a JSON string
- `messageHandler:` -- Set the callback block for incoming messages

### Message Parsing Layer

Handles JSON-RPC 2.0 serialization/deserialization:

- **`McpJsonRpcMessage`**: Abstract base with factory method `fromDictionary:` that automatically determines message type (request, response, notification, or error) and returns the appropriate subclass.
- **`McpJsonRpcRequest`**: Has `id`, `method`, and optional `params`. Represents a client request expecting a response.
- **`McpJsonRpcResponse`**: Has `id` and either `result` or `error`. Represents a server response.
- **`McpJsonRpcNotification`**: Has `method` and optional `params` but no `id`. Fire-and-forget messages.
- **`McpJsonRpcError`**: Structured error with `code`, `message`, and optional `data`. Includes factory methods for standard JSON-RPC errors.

### Server Layer

**`McpServer`** is the central coordinator:

1. Receives raw JSON via the transport's message handler callback
2. Parses the JSON into a `McpJsonRpcMessage`
3. Routes requests to the appropriate handler method based on the `method` field
4. Serializes responses and sends them back through the transport

Supported MCP methods:

| Method | Handler | Description |
|--------|---------|-------------|
| `initialize` | `handleInitialize:` | Protocol handshake |
| `ping` | `handlePing:` | Health check |
| `tools/list` | `handleToolsList:` | List registered tools |
| `tools/call` | `handleToolsCall:` | Execute a tool |
| `resources/list` | `handleResourcesList:` | List registered resources |
| `resources/read` | `handleResourcesRead:` | Read a resource by URI |
| `resources/templates/list` | `handleResourcesTemplatesList:` | List resource templates |
| `prompts/list` | `handlePromptsList:` | List registered prompts |
| `prompts/get` | `handlePromptsGet:` | Get a prompt with arguments |

Notifications handled:

| Notification | Description |
|-------------|-------------|
| `notifications/initialized` | Client confirms initialization complete |

### Domain Layer

**Tools** (`McpTool`): Executable operations with:
- Name and description
- Input schema (`McpSchema`) defining expected parameters
- Handler block that receives arguments and returns `McpToolResult`

**Resources** (`McpResource`): Static data endpoints with:
- URI, name, description, MIME type
- Handler block that returns an array of `McpContentItem`

**Resource Templates** (`McpResourceTemplate`): Dynamic URI-based resources with:
- URI template with `{param}` placeholders
- Parameter extraction from concrete URIs
- Handler block receiving extracted parameters

**Prompts** (`McpPrompt`): Structured prompt templates with:
- Name, description, and argument specifications
- Handler block that returns messages for the LLM

## Design Decisions

### Block-Based Handlers

All tools, resources, and prompts use Smalltalk blocks as handlers. This leverages Pharo's closure support for maximum flexibility -- handlers can capture any context from their defining scope.

### Error Isolation

Tool and resource handlers are wrapped in error handlers. If a tool handler raises an exception, it's caught and returned as an `McpToolResult` error rather than crashing the server.

### In-Memory Transport for Testing

The `McpInMemoryTransport` allows the entire MCP protocol to be tested without stdio, making tests fast, deterministic, and isolated. The `simulateReceive:` method triggers the full server message handling pipeline.

### Ordered Dictionaries

All `asDictionary` methods use `OrderedDictionary` to produce deterministic JSON output, which aids debugging and testing.