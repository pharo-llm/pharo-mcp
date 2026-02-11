# Transport Guide

The transport layer handles the physical communication between the MCP client (LLM) and the server (Pharo). LLM-Pharo-MCP ships with two transports.

## Transport Architecture

All transports extend `McpTransport` and implement three methods:

| Method | Description |
|--------|-------------|
| `start` | Begin listening for messages |
| `stop` | Stop listening |
| `sendMessage:` | Send a JSON string to the client |

The transport communicates incoming messages to the server via the `messageHandler:` callback, which the server sets during `start`.

## McpStdioTransport

The standard transport for production use. Reads JSON-RPC messages line-by-line from stdin and writes responses to stdout.

### Setup

```smalltalk
transport := McpStdioTransport stdin: Stdio stdin stdout: Stdio stdout.
server := McpServer default.
server transport: transport.
server start.
```

### How It Works

1. On `start`, spawns a background process that reads lines from the input stream
2. Each non-empty line is passed to the `messageHandler` callback
3. `sendMessage:` writes the JSON string to the output stream followed by a newline, then flushes

### Lifecycle

```smalltalk
"Start"
server start.
"Check status"
transport isRunning. "=> true"
"Stop"
server stop.
transport isRunning. "=> false"
```

### Integration with AI Clients

When configured as an MCP server in an AI client (like Claude Desktop), the client launches the Pharo process and communicates via stdio:

```json
{
  "mcpServers": {
    "pharo": {
      "command": "/path/to/pharo",
      "args": ["--headless", "my-image.image", "eval", "McpServer startStdio"]
    }
  }
}
```

## McpInMemoryTransport

A transport designed for testing that stores messages in memory and allows programmatic message injection.

### Setup

```smalltalk
transport := McpInMemoryTransport new.
server := McpServer default.
server transport: transport.
server start.
```

### Simulating Client Messages

```smalltalk
"Simulate a client sending a request"
transport simulateReceive: '{"jsonrpc":"2.0","id":1,"method":"ping"}'.
"Check the server's response"
transport lastSentMessage.
"=> '{"jsonrpc":"2.0","id":1,"result":{}}'"
transport lastSentParsed.
"=> a Dictionary( 'jsonrpc'->'2.0' 'id'->1 'result'->a Dictionary() )"
```

### Inspecting All Messages

```smalltalk
"All messages sent by the server"
transport sentMessages.
"Clear the message history"
transport reset.
```

### Testing Pattern

The test suite uses `McpInMemoryTransport` extensively:

```smalltalk
McpServerTest >> setUp [
  server := McpServer name: 'test-server' version: '1.0.0'.
  transport := McpInMemoryTransport new.
  server transport: transport.
  server start
]
McpServerTest >> testPing [
  | request response |
  request := McpJsonRpcRequest id: 1 method: 'ping'.
  transport simulateReceive: request asJsonString.
  response := transport lastSentParsed.
  self assert: (response at: 'id') equals: 1.
  self assert: (response at: 'result') equals: Dictionary new
]
```

## Creating a Custom Transport

To implement a custom transport (HTTP, WebSocket, etc.), subclass `McpTransport`:

```smalltalk
McpTransport subclass: #McpHttpTransport
  instanceVariableNames: 'httpServer port'
  classVariableNames: ''
  package: 'MyPackage'.
McpHttpTransport >> start [
  httpServer := ZnServer startDefaultOn: port.
  httpServer
    onRequestRespond: [ :req |
      | body |
      body := req entity string.
      messageHandler value: body.
      "Return the last response (simplified)"
      ZnResponse ok: (ZnEntity json: self lastResponse) ]
]
McpHttpTransport >> stop [
  httpServer stop
]
McpHttpTransport >> sendMessage: aJsonString [
  "Store for the HTTP response"
  lastResponse := aJsonString
]
```