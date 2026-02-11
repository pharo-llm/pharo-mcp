# API Reference

Complete class and method reference for LLM-Pharo-MCP.

---

## McpServer

The main MCP server class. Manages tools, resources, prompts, and handles protocol messages.

### Class Methods

| Method | Description |
|--------|-------------|
| `McpServer class >> name: version:` | Create a server with a given name and version |
| `McpServer class >> default` | Create a server with default name and version from `McpConstants` |

### Instance Methods -- Configuration

| Method | Description |
|--------|-------------|
| `name` / `name:` | Get/set the server name |
| `version` / `version:` | Get/set the server version |
| `capabilities` / `capabilities:` | Get/set the `McpServerCapabilities` |
| `transport` / `transport:` | Get/set the transport |

### Instance Methods -- Tools

| Method | Description |
|--------|-------------|
| `addTool:` | Register an `McpTool` |
| `removeTool:` | Remove a tool by name |
| `toolNamed:` | Look up a tool by name (returns nil if not found) |
| `tools` | Get the tools dictionary |

### Instance Methods -- Resources

| Method | Description |
|--------|-------------|
| `addResource:` | Register an `McpResource` |
| `removeResource:` | Remove a resource by URI |
| `resourceAtUri:` | Look up a resource by URI (returns nil if not found) |
| `resources` | Get the resources dictionary |
| `addResourceTemplate:` | Register an `McpResourceTemplate` |
| `resourceTemplates` | Get the resource templates dictionary |

### Instance Methods -- Prompts

| Method | Description |
|--------|-------------|
| `addPrompt:` | Register an `McpPrompt` |
| `removePrompt:` | Remove a prompt by name |
| `promptNamed:` | Look up a prompt by name (returns nil if not found) |
| `prompts` | Get the prompts dictionary |

### Instance Methods -- Lifecycle

| Method | Description |
|--------|-------------|
| `start` | Start the server (transport must be set) |
| `stop` | Stop the server |
| `isInitialized` | Whether the client has sent `notifications/initialized` |

---

## McpTool

Represents an executable tool.

### Class Methods

| Method | Description |
|--------|-------------|
| `name: description: inputSchema: handler:` | Create a tool with full schema |
| `name: description: handler:` | Create a tool with empty schema (no params) |

### Instance Methods

| Method | Description |
|--------|-------------|
| `name` / `name:` | Get/set the tool name |
| `description` / `description:` | Get/set the tool description |
| `inputSchema` / `inputSchema:` | Get/set the input `McpSchema` |
| `handler` / `handler:` | Get/set the handler block `[ :args | ... ]` |
| `executeWith:` | Execute the tool with an arguments dictionary. Returns `McpToolResult` |
| `asDictionary` | Convert to MCP protocol dictionary |

---

## McpSchema

JSON Schema representation for tool input validation.

### Class Methods -- Primitives

| Method | Description |
|--------|-------------|
| `string` | String type schema |
| `string:` | String type with description |
| `number` | Number type schema |
| `number:` | Number type with description |
| `integer` | Integer type schema |
| `integer:` | Integer type with description |
| `boolean` | Boolean type schema |
| `boolean:` | Boolean type with description |

### Class Methods -- Complex Types

| Method | Description |
|--------|-------------|
| `object:` | Object schema from property associations `{ 'name' -> McpSchema string }` |
| `object: required:` | Object schema with required field names |
| `array:` | Array schema with item schema |
| `enum:` | String enum schema with allowed values |

### Instance Methods

| Method | Description |
|--------|-------------|
| `type` / `type:` | Get/set the JSON Schema type |
| `properties` / `properties:` | Get/set properties (for object type) |
| `required` / `required:` | Get/set required field names (for object type) |
| `description` / `description:` | Get/set description |
| `items` / `items:` | Get/set items schema (for array type) |
| `enum` / `enum:` | Get/set enum values |
| `asDictionary` | Convert to JSON Schema dictionary |

---

## McpResource

Represents a static resource with a fixed URI.

### Class Methods

| Method | Description |
|--------|-------------|
| `uri: name: description: mimeType: handler:` | Full constructor |
| `uri: name: description: handler:` | Constructor with default `text/plain` MIME type |

### Instance Methods

| Method | Description |
|--------|-------------|
| `uri` / `uri:` | Get/set the resource URI |
| `name` / `name:` | Get/set the resource name |
| `description` / `description:` | Get/set the description |
| `mimeType` / `mimeType:` | Get/set the MIME type |
| `handler` / `handler:` | Get/set the handler block `[ ^ { McpContentItem ... } ]` |
| `read` | Execute the handler. Returns array of `McpContentItem` |
| `asDictionary` | Convert to MCP protocol dictionary |

---

## McpResourceTemplate

Represents a dynamic resource with a URI template containing `{param}` placeholders.

### Class Methods

| Method | Description |
|--------|-------------|
| `uriTemplate: name: description: mimeType: handler:` | Full constructor |
| `uriTemplate: name: description: handler:` | Constructor with default `text/plain` MIME type |

### Instance Methods

| Method | Description |
|--------|-------------|
| `uriTemplate` / `uriTemplate:` | Get/set the URI template |
| `name` / `name:` | Get/set the name |
| `description` / `description:` | Get/set the description |
| `mimeType` / `mimeType:` | Get/set the MIME type |
| `handler` / `handler:` | Get/set the handler block `[ :params | ... ]` |
| `extractParams:` | Extract parameters from a URI. Returns a Dictionary or nil |
| `matchesUri:` | Check if a URI matches this template |
| `readWithUri:` | Read by extracting params from URI and calling handler |
| `asDictionary` | Convert to MCP protocol dictionary |

---

## McpPrompt

Represents a prompt template.

### Class Methods

| Method | Description |
|--------|-------------|
| `name: description: arguments: handler:` | Full constructor with arguments |
| `name: description: handler:` | Constructor with no arguments |

### Instance Methods

| Method | Description |
|--------|-------------|
| `name` / `name:` | Get/set the prompt name |
| `description` / `description:` | Get/set the description |
| `arguments` / `arguments:` | Get/set the array of `McpPromptArgument` |
| `handler` / `handler:` | Get/set the handler block `[ :args | ... ]` |
| `getWithArguments:` | Execute with arguments dictionary. Returns result dictionary |
| `asDictionary` | Convert to MCP protocol dictionary |

---

## McpPromptArgument

Defines an argument for a prompt.

### Class Methods

| Method | Description |
|--------|-------------|
| `name: description:` | Create an optional argument |
| `name: description: required:` | Create with explicit required flag |
| `required: description:` | Create a required argument (first param is the name) |

### Instance Methods

| Method | Description |
|--------|-------------|
| `name` / `name:` | Get/set the argument name |
| `description` / `description:` | Get/set the description |
| `required` / `required:` | Get/set the required flag |
| `isRequired` | Check if this argument is required |
| `asDictionary` | Convert to dictionary |

---

## McpPromptMessage

A message within a prompt (user or assistant role).

### Class Methods

| Method | Description |
|--------|-------------|
| `user:` | Create a user message with text content |
| `assistant:` | Create an assistant message with text content |
| `role: content:` | Create with custom role and `McpContentItem` |

### Instance Methods

| Method | Description |
|--------|-------------|
| `role` / `role:` | Get/set the role (`'user'` or `'assistant'`) |
| `content` / `content:` | Get/set the `McpContentItem` |
| `asDictionary` | Convert to dictionary |

---

## McpContentItem

Represents content in tool results, resource reads, and prompt messages.

### Class Methods

| Method | Description |
|--------|-------------|
| `text:` | Create text content |
| `image: mimeType:` | Create image content (base64 data) |
| `resource: text: mimeType:` | Create embedded resource content |

### Instance Methods

| Method | Description |
|--------|-------------|
| `type` / `type:` | Get/set the content type (`'text'`, `'image'`, `'resource'`) |
| `text` / `text:` | Get/set text content |
| `data` / `data:` | Get/set base64 image data |
| `mimeType` / `mimeType:` | Get/set MIME type |
| `uri` / `uri:` | Get/set resource URI |
| `isText` / `isImage` / `isResource` | Type checks |
| `asDictionary` | Convert to MCP content dictionary |

---

## McpToolResult

Result returned from tool execution.

### Class Methods

| Method | Description |
|--------|-------------|
| `successText:` | Create success result with text content |
| `errorText:` | Create error result with text content |
| `success:` | Create success result with content items array |
| `error:` | Create error result with content items array |

### Instance Methods

| Method | Description |
|--------|-------------|
| `content` / `content:` | Get/set the content items array |
| `isError` / `isError:` | Get/set the error flag |
| `asDictionary` | Convert to MCP result dictionary |

---

## McpServerCapabilities

Server capability flags advertised during initialization.

### Instance Methods

| Method | Description |
|--------|-------------|
| `supportsTools` / `supportsTools:` | Get/set tools capability (default: true) |
| `supportsResources` / `supportsResources:` | Get/set resources capability (default: true) |
| `supportsPrompts` / `supportsPrompts:` | Get/set prompts capability (default: true) |
| `supportsResourceSubscriptions` / `supportsResourceSubscriptions:` | Get/set subscription support (default: false) |
| `asDictionary` | Convert to MCP capabilities dictionary |

---

## McpConstants

Shared pool with protocol constants.

### Class Methods

| Method | Value | Description |
|--------|-------|-------------|
| `mcpProtocolVersion` | `'2025-03-26'` | MCP protocol version |
| `mcpServerName` | `'pharo-mcp'` | Default server name |
| `mcpServerVersion` | `'0.1.0'` | Default server version |
| `errorCodeParseError` | `-32700` | JSON parse error |
| `errorCodeInvalidRequest` | `-32600` | Invalid JSON-RPC request |
| `errorCodeMethodNotFound` | `-32601` | Method not found |
| `errorCodeInvalidParams` | `-32602` | Invalid parameters |
| `errorCodeInternalError` | `-32603` | Internal server error |

---

## Transport Classes

### McpTransport (Abstract)

| Method | Description |
|--------|-------------|
| `messageHandler:` | Set the incoming message callback |
| `start` | Start the transport |
| `stop` | Stop the transport |
| `sendMessage:` | Send a JSON string |

### McpStdioTransport

Extends `McpTransport` for stdin/stdout communication.

| Method | Description |
|--------|-------------|
| `McpStdioTransport class >> stdin: stdout:` | Create with input/output streams |
| `isRunning` | Check if the read loop is active |

### McpInMemoryTransport

Extends `McpTransport` for testing.

| Method | Description |
|--------|-------------|
| `sentMessages` | Get all sent messages |
| `lastSentMessage` | Get last sent JSON string |
| `lastSentParsed` | Get last sent message parsed as dictionary |
| `simulateReceive:` | Simulate receiving a message |
| `reset` | Clear sent messages |

---

## JSON-RPC Classes

### McpJsonRpcMessage (Abstract)

| Method | Description |
|--------|-------------|
| `McpJsonRpcMessage class >> fromDictionary:` | Parse a dictionary into the correct subclass |
| `McpJsonRpcMessage class >> fromJsonString:` | Parse a JSON string into the correct subclass |
| `jsonrpc` | Returns `'2.0'` |
| `asDictionary` | Convert to dictionary (abstract) |
| `asJsonString` | Convert to JSON string |

### McpJsonRpcRequest

| Method | Description |
|--------|-------------|
| `id` / `method` / `params` | Request fields |
| `isRequest` | Returns true |

### McpJsonRpcResponse

| Method | Description |
|--------|-------------|
| `id` / `result` / `error` | Response fields |
| `isSuccess` / `isError` | Check response type |

### McpJsonRpcNotification

| Method | Description |
|--------|-------------|
| `method` / `params` | Notification fields |
| `isNotification` | Returns true |

### McpJsonRpcError

| Method | Description |
|--------|-------------|
| `code` / `message` / `data` | Error fields |
| Factory methods | `parseError`, `invalidRequest`, `methodNotFound`, `methodNotFound:`, `invalidParams`, `invalidParams:`, `internalError`, `internalError:` |