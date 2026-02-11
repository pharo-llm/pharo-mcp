Class {
	#name : 'MCPConstants',
	#superclass : 'Object',
	#classInstVars : [
		'errorCodeInternalError',
		'errorCodeInvalidParams',
		'errorCodeInvalidRequest',
		'errorCodeMethodNotFound',
		'errorCodeParseError',
		'mcpProtocolVersion',
		'mcpServerName',
		'mcpServerVersion'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'class initialization' }
MCPConstants class >> errorCodeInternalError [

	^ errorCodeInternalError
]

{ #category : 'class initialization' }
MCPConstants class >> errorCodeInvalidParams [

	^ errorCodeInvalidParams
]

{ #category : 'class initialization' }
MCPConstants class >> errorCodeInvalidRequest [

	^ errorCodeInvalidRequest
]

{ #category : 'class initialization' }
MCPConstants class >> errorCodeMethodNotFound [

	^ errorCodeMethodNotFound
]

{ #category : 'class initialization' }
MCPConstants class >> errorCodeParseError [

	^ errorCodeParseError
]

{ #category : 'class initialization' }
MCPConstants class >> initialize [

	mcpProtocolVersion := '2025-03-26'.
	mcpServerName := 'pharo-mcp'.
	mcpServerVersion := '0.1.0'.
	errorCodeParseError := -32700.
	errorCodeInvalidRequest := -32600.
	errorCodeMethodNotFound := -32601.
	errorCodeInvalidParams := -32602.
	errorCodeInternalError := -32603
]

{ #category : 'class initialization' }
MCPConstants class >> mcpProtocolVersion [

	^ mcpProtocolVersion
]

{ #category : 'class initialization' }
MCPConstants class >> mcpServerName [

	^ mcpServerName
]

{ #category : 'class initialization' }
MCPConstants class >> mcpServerVersion [

	^ mcpServerVersion
]
