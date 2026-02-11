# Pharo-MCP

[![Pharo 13 & 14](https://img.shields.io/badge/Pharo-13%20%7C%2014-2c98f0.svg)](https://github.com/pharo-llm/pharo-mcp)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://github.com/pharo-llm/pharo-mcp/blob/master/LICENSE)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/pharo-llm/pharo-mcp/pulls)
[![Status: Active](https://img.shields.io/badge/status-active-success.svg)](https://github.com/pharo-llm/pharo-mcp)
[![CI](https://github.com/omarabedelkader/ChatPharo/actions/workflows/CI.yml/badge.svg)](https://github.com/pharo-llm/pharo-mcp/actions/workflows/CI.yml)


Pharo-MCP implements the **Model Context Protocol (MCP)** in Pharo so that Pharo-based tools and agents can use the standardized MCP framework to connect AI models with external data sources and tools in a uniform way. ([https://modelcontextprotocol.io/](https://modelcontextprotocol.io/))

To install stable version of `Pharo-ACP` in your image you can use:

```smalltalk
Metacello new
  githubUser: 'pharo-llm' project: 'pharo-mcp' commitish: 'X.X.X' path: 'src';
  baseline: 'LLMPharoMCP';
  load
```


For development version install it with this:

```smalltalk
Metacello new
  githubUser: 'pharo-llm' project: 'pharo-mcp' commitish: 'main' path: 'src';
  baseline: 'LLMPharoMCP';
  load.
```
