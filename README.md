# LLM-Pharo-MCP

A **Model Context Protocol (MCP)** server implementation for [Pharo Smalltalk](https://pharo.org/), enabling AI language models to interact with live Pharo environments through a standardized protocol.

[![CI](https://github.com/pharo-llm/pharo-mcp/actions/workflows/CI.yml/badge.svg)](https://github.com/pharo-llm/pharo-mcp/actions/workflows/CI.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

## Overview

LLM-Pharo-MCP implements the [Model Context Protocol](https://modelcontextprotocol.io/) specification, providing a bridge between AI assistants (Claude, GPT, etc.) and live Pharo Smalltalk images. Through MCP, AI models can:

- **Execute tools** defined in Pharo (evaluate code, inspect objects, manage packages)
- **Read resources** exposed by the Pharo environment (system info, class hierarchies, source code)
- **Use prompts** for structured interactions (code review, refactoring, documentation generation)

## Installation

### In a Pharo Image

```smalltalk
Metacello new
  baseline: 'LLMPharoMCP';
  repository: 'github://pharo-llm/pharo-mcp:main/src';
  load.
```

To load only the core (without tests):

```smalltalk
Metacello new
  baseline: 'LLMPharoMCP';
  repository: 'github://pharo-llm/pharo-mcp:main/src';
  load: 'Core'.
```

### Supported Platforms

- Pharo 13 (64-bit)
- Pharo 14 (64-bit)
- macOS, Linux, Windows

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

## Documentation

| Document | Description |
|----------|-------------|
| [Architecture](docs/architecture.md) | System architecture and design decisions |
| [API Reference](docs/api-reference.md) | Complete class and method reference |
| [Protocol Guide](docs/protocol-guide.md) | MCP protocol details and message formats |
| [Tools Guide](docs/tools-guide.md) | How to create and register tools |
| [Resources Guide](docs/resources-guide.md) | How to expose resources and resource templates |
| [Prompts Guide](docs/prompts-guide.md) | How to define and use prompts |
| [Transport Guide](docs/transport-guide.md) | Transport layer configuration |
| [Testing Guide](docs/testing-guide.md) | How to test your MCP server and extensions |

## Project Structure

```
src/
  BaselineOfLLMPharoMCP/       -- Metacello baseline
  LLM-Pharo-MCP/               -- Core implementation
    McpServer                   -- Main MCP server
    McpTool                     -- Tool definitions
    McpResource                 -- Static resource definitions
    McpResourceTemplate         -- Dynamic resource URI templates
    McpPrompt                   -- Prompt definitions
    McpPromptArgument           -- Prompt argument specifications
    McpPromptMessage            -- Prompt messages (user/assistant)
    McpSchema                   -- JSON Schema for tool inputs
    McpContentItem              -- Content items (text, image, resource)
    McpToolResult               -- Tool execution results
    McpTransport                -- Abstract transport
    McpStdioTransport           -- stdio-based transport
    McpInMemoryTransport        -- In-memory transport (for testing)
    McpJsonRpcMessage           -- JSON-RPC message base + parser
    McpJsonRpcRequest           -- JSON-RPC request
    McpJsonRpcResponse          -- JSON-RPC response
    McpJsonRpcNotification      -- JSON-RPC notification
    McpJsonRpcError             -- JSON-RPC error
    McpServerCapabilities       -- Server capability configuration
    McpConstants                -- Protocol constants and error codes
  LLM-Pharo-MCP-Tests/         -- Test suite
    McpServerTest               -- Server integration tests (35+ tests)
    McpToolTest                 -- Tool tests
    McpResourceTest             -- Resource tests
    McpResourceTemplateTest     -- Resource template tests
    McpPromptTest               -- Prompt tests
    McpPromptArgumentTest       -- Prompt argument tests
    McpPromptMessageTest        -- Prompt message tests
    McpSchemaTest               -- JSON Schema tests
    McpContentItemTest          -- Content item tests
    McpToolResultTest           -- Tool result tests
    McpJsonRpcMessageTest       -- JSON-RPC message parsing tests
    McpJsonRpcRequestTest       -- JSON-RPC request tests
    McpJsonRpcResponseTest      -- JSON-RPC response tests
    McpJsonRpcNotificationTest  -- JSON-RPC notification tests
    McpJsonRpcErrorTest         -- JSON-RPC error tests
    McpServerCapabilitiesTest   -- Capabilities tests
    McpInMemoryTransportTest    -- In-memory transport tests
    McpConstantsTest            -- Constants tests
```

## Running Tests

In a Pharo image with the project loaded:

```smalltalk
"Run all tests"
(TestSuite forPackage: 'LLM-Pharo-MCP-Tests') run.
```

From the command line with SmalltalkCI:

```bash
smalltalkci -s Pharo64-14
```

## MCP Protocol Version

This implementation targets **MCP protocol version `2025-03-26`**.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Contributing

1. Fork the repository
2. Create your feature branch
3. Write tests for your changes
4. Ensure all tests pass
5. Submit a pull request

## Links

- [Model Context Protocol Specification](https://modelcontextprotocol.io/)
- [Pharo Smalltalk](https://pharo.org/)
- [MCP Protocol Schema](https://github.com/modelcontextprotocol/specification)
