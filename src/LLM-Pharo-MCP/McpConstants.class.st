Class {
	#name : 'McpConstants',
	#superclass : 'SharedPool',
	#classVars : [
		'ErrorCodeInternalError',
		'ErrorCodeInvalidParams',
		'ErrorCodeInvalidRequest',
		'ErrorCodeMethodNotFound',
		'ErrorCodeParseError',
		'McpProtocolVersion',
		'McpServerName',
		'McpServerVersion'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'class initialization' }
McpConstants class >> initialize [

	McpProtocolVersion := '2025-03-26'.
	McpServerName := 'pharo-mcp'.
	McpServerVersion := '0.1.0'.
	ErrorCodeParseError := -32700.
	ErrorCodeInvalidRequest := -32600.
	ErrorCodeMethodNotFound := -32601.
	ErrorCodeInvalidParams := -32602.
	ErrorCodeInternalError := -32603
]

{ #category : 'accessing' }
McpConstants class >> errorCodeInternalError [

	^ ErrorCodeInternalError
]

{ #category : 'accessing' }
McpConstants class >> errorCodeInvalidParams [

	^ ErrorCodeInvalidParams
]

{ #category : 'accessing' }
McpConstants class >> errorCodeInvalidRequest [

	^ ErrorCodeInvalidRequest
]

{ #category : 'accessing' }
McpConstants class >> errorCodeMethodNotFound [

	^ ErrorCodeMethodNotFound
]

{ #category : 'accessing' }
McpConstants class >> errorCodeParseError [

	^ ErrorCodeParseError
]

{ #category : 'accessing' }
McpConstants class >> mcpProtocolVersion [

	^ McpProtocolVersion
]

{ #category : 'accessing' }
McpConstants class >> mcpServerName [

	^ McpServerName
]

{ #category : 'accessing' }
McpConstants class >> mcpServerVersion [

	^ McpServerVersion
]
