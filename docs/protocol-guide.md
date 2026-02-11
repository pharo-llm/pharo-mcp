# MCP Protocol Guide

This document describes the Model Context Protocol (MCP) as implemented by LLM-Pharo-MCP, including message formats, the initialization handshake, and all supported operations.

## Protocol Basics

MCP uses **JSON-RPC 2.0** as its wire protocol. All messages are JSON objects with a `"jsonrpc": "2.0"` field.

### Message Types

**Request** (client to server, expects response):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list",
  "params": {}
}
```

**Response** (server to client):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "tools": []
  }
}
```

**Error Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32601,
    "message": "Method not found: unknown/method"
  }
}
```

**Notification** (client to server, no response):
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

## Initialization Handshake

Every MCP session begins with a two-step handshake:

### Step 1: `initialize` Request

The client sends an `initialize` request with its capabilities:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-03-26",
    "capabilities": {},
    "clientInfo": {
      "name": "claude-desktop",
      "version": "1.0.0"
    }
  }
}
```

The server responds with its own capabilities and info:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2025-03-26",
    "capabilities": {
      "tools": { "listChanged": false },
      "resources": { "subscribe": false, "listChanged": false },
      "prompts": { "listChanged": false }
    },
    "serverInfo": {
      "name": "pharo-mcp",
      "version": "0.1.0"
    }
  }
}
```

### Step 2: `notifications/initialized` Notification

The client confirms initialization is complete:

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

After this, the server marks itself as initialized and is ready to serve requests.

## Supported Methods

### `ping`

Health check. Returns an empty result.

**Request:**
```json
{ "jsonrpc": "2.0", "id": 2, "method": "ping" }
```

**Response:**
```json
{ "jsonrpc": "2.0", "id": 2, "result": {} }
```

---

### `tools/list`

List all registered tools.

**Request:**
```json
{ "jsonrpc": "2.0", "id": 3, "method": "tools/list" }
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "tools": [
      {
        "name": "evaluate",
        "description": "Evaluate a Pharo expression",
        "inputSchema": {
          "type": "object",
          "properties": {
            "expression": {
              "type": "string",
              "description": "Pharo expression to evaluate"
            }
          },
          "required": ["expression"]
        }
      }
    ]
  }
}
```

---

### `tools/call`

Execute a tool by name with arguments.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "evaluate",
    "arguments": {
      "expression": "3 + 4"
    }
  }
}
```

**Success Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "content": [
      { "type": "text", "text": "7" }
    ]
  }
}
```

**Error Response (tool execution error):**
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "content": [
      { "type": "text", "text": "Error: undefined variable" }
    ],
    "isError": true
  }
}
```

---

### `resources/list`

List all registered static resources.

**Request:**
```json
{ "jsonrpc": "2.0", "id": 5, "method": "resources/list" }
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "resources": [
      {
        "uri": "pharo://system/version",
        "name": "System Version",
        "description": "Current Pharo version",
        "mimeType": "text/plain"
      }
    ]
  }
}
```

---

### `resources/read`

Read a resource by URI. Tries exact match first, then resource templates.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "resources/read",
  "params": {
    "uri": "pharo://system/version"
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {
    "contents": [
      { "type": "text", "text": "Pharo 13.0" }
    ]
  }
}
```

---

### `resources/templates/list`

List all registered resource templates.

**Request:**
```json
{ "jsonrpc": "2.0", "id": 7, "method": "resources/templates/list" }
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "result": {
    "resourceTemplates": [
      {
        "uriTemplate": "pharo://class/{className}",
        "name": "Class Details",
        "description": "Get details for a Pharo class",
        "mimeType": "application/json"
      }
    ]
  }
}
```

---

### `prompts/list`

List all registered prompts.

**Request:**
```json
{ "jsonrpc": "2.0", "id": 8, "method": "prompts/list" }
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "result": {
    "prompts": [
      {
        "name": "review-class",
        "description": "Review a Pharo class",
        "arguments": [
          {
            "name": "className",
            "description": "The class to review",
            "required": true
          }
        ]
      }
    ]
  }
}
```

---

### `prompts/get`

Get a prompt with resolved arguments.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "method": "prompts/get",
  "params": {
    "name": "review-class",
    "arguments": {
      "className": "OrderedCollection"
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "result": {
    "description": "Code review for OrderedCollection",
    "messages": [
      {
        "role": "user",
        "content": {
          "type": "text",
          "text": "Please review the class OrderedCollection."
        }
      }
    ]
  }
}
```

## Error Codes

| Code | Name | Description |
|------|------|-------------|
| `-32700` | Parse Error | Invalid JSON |
| `-32600` | Invalid Request | Not a valid JSON-RPC request |
| `-32601` | Method Not Found | Unknown MCP method |
| `-32602` | Invalid Params | Missing or invalid parameters |
| `-32603` | Internal Error | Server-side error |
