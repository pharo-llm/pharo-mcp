Class {
	#name : 'McpConstantsTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpConstantsTest >> testProtocolVersion [

	self assert: McpConstants mcpProtocolVersion equals: '2025-03-26'
]

{ #category : 'tests' }
McpConstantsTest >> testServerName [

	self assert: McpConstants mcpServerName equals: 'pharo-mcp'
]

{ #category : 'tests' }
McpConstantsTest >> testServerVersion [

	self assert: McpConstants mcpServerVersion equals: '0.1.0'
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeParseError [

	self assert: McpConstants errorCodeParseError equals: -32700
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeInvalidRequest [

	self assert: McpConstants errorCodeInvalidRequest equals: -32600
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeMethodNotFound [

	self assert: McpConstants errorCodeMethodNotFound equals: -32601
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeInvalidParams [

	self assert: McpConstants errorCodeInvalidParams equals: -32602
]

{ #category : 'tests' }
McpConstantsTest >> testErrorCodeInternalError [

	self assert: McpConstants errorCodeInternalError equals: -32603
]
