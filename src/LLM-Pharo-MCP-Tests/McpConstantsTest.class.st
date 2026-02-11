Class {
	#name : 'McpConstantsTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeInternalError [

	self assert: MCPConstants errorCodeInternalError equals: -32603
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeInvalidParams [

	self assert: MCPConstants errorCodeInvalidParams equals: -32602
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeInvalidRequest [

	self assert: MCPConstants errorCodeInvalidRequest equals: -32600
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeMethodNotFound [

	self assert: MCPConstants errorCodeMethodNotFound equals: -32601
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeParseError [

	self assert: MCPConstants errorCodeParseError equals: -32700
]

{ #category : 'tests' }
McpConstantsTest >> testProtocolVersion [

	self assert: MCPConstants mcpProtocolVersion equals: '2025-03-26'
]

{ #category : 'tests' }
McpConstantsTest >> testServerName [

	self assert: MCPConstants mcpServerName equals: 'pharo-mcp'

]
