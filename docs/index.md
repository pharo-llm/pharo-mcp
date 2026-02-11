# LLM-Pharo-MCP

A **Model Context Protocol (MCP)** server implementation for [Pharo Smalltalk](https://pharo.org/), enabling AI language models to interact with live Pharo environments through a standardized protocol.

## Overview

LLM-Pharo-MCP implements the [Model Context Protocol](https://modelcontextprotocol.io/) specification, providing a bridge between AI assistants (Claude, GPT, etc.) and live Pharo Smalltalk images. Through MCP, AI models can:

- **Execute tools** defined in Pharo (evaluate code, inspect objects, manage packages)
- **Read resources** exposed by the Pharo environment (system info, class hierarchies, source code)
- **Use prompts** for structured interactions (code review, refactoring, documentation generation)

### Supported Platforms

- Pharo 13 (64-bit)
- Pharo 14 (64-bit)
- macOS, Linux, Windows

## Documentation

| Document | Description |
|----------|-------------|
| [Quick Start](quick-start.md) | System architecture and design decisions |
| [Architecture](architecture.md) | System architecture and design decisions |
| [API Reference](api-reference.md) | Complete class and method reference |
| [Protocol Guide](protocol-guide.md) | MCP protocol details and message formats |
| [Tools Guide](tools-guide.md) | How to create and register tools |
| [Resources Guide](resources-guide.md) | How to expose resources and resource templates |
| [Prompts Guide](prompts-guide.md) | How to define and use prompts |
| [Transport Guide](transport-guide.md) | Transport layer configuration |
| [Testing Guide](testing-guide.md) | How to test your MCP server and extensions |


## MCP Protocol Version

This implementation targets **MCP protocol version `2025-03-26`**.

## Ressources

- [Model Context Protocol Specification](https://modelcontextprotocol.io/)
- [Pharo Smalltalk](https://pharo.org/)
- [MCP Protocol Schema](https://github.com/modelcontextprotocol/specification)
