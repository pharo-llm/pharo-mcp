Class {
	#name : 'McpJsonRpcErrorTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpJsonRpcErrorTest >> testAsDictionary [

	| error dict |
	error := McpJsonRpcError code: -32600 message: 'Invalid request'.
	dict := error asDictionary.
	self assert: (dict at: 'code') equals: -32600.
	self assert: (dict at: 'message') equals: 'Invalid request'.
	self deny: (dict includesKey: 'data')
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testAsDictionaryWithData [

	| error dict |
	error := McpJsonRpcError code: -32603 message: 'Error' data: 'details'.
	dict := error asDictionary.
	self assert: (dict at: 'code') equals: -32603.
	self assert: (dict at: 'message') equals: 'Error'.
	self assert: (dict at: 'data') equals: 'details'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testCreateWithCodeAndMessage [

	| error |
	error := McpJsonRpcError code: -32600 message: 'Invalid request'.
	self assert: error code equals: -32600.
	self assert: error message equals: 'Invalid request'.
	self assert: error data isNil
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testCreateWithCodeMessageAndData [

	| error |
	error := McpJsonRpcError code: -32603 message: 'Internal error' data: 'stack trace here'.
	self assert: error code equals: -32603.
	self assert: error message equals: 'Internal error'.
	self assert: error data equals: 'stack trace here'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testInternalError [

	| error |
	error := McpJsonRpcError internalError.
	self assert: error code equals: -32603.
	self assert: error message equals: 'Internal error'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testInternalErrorWithMessage [

	| error |
	error := McpJsonRpcError internalError: 'Something went wrong'.
	self assert: error code equals: -32603.
	self assert: error message equals: 'Something went wrong'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testInvalidParams [

	| error |
	error := McpJsonRpcError invalidParams.
	self assert: error code equals: -32602.
	self assert: error message equals: 'Invalid params'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testInvalidParamsWithMessage [

	| error |
	error := McpJsonRpcError invalidParams: 'Missing name'.
	self assert: error code equals: -32602.
	self assert: error message equals: 'Missing name'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testInvalidRequest [

	| error |
	error := McpJsonRpcError invalidRequest.
	self assert: error code equals: -32600.
	self assert: error message equals: 'Invalid request'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testMethodNotFound [

	| error |
	error := McpJsonRpcError methodNotFound.
	self assert: error code equals: -32601.
	self assert: error message equals: 'Method not found'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testMethodNotFoundWithName [

	| error |
	error := McpJsonRpcError methodNotFound: 'foo/bar'.
	self assert: error code equals: -32601.
	self assert: error message equals: 'Method not found: foo/bar'
]

{ #category : 'tests' }
McpJsonRpcErrorTest >> testParseError [

	| error |
	error := McpJsonRpcError parseError.
	self assert: error code equals: -32700.
	self assert: error message equals: 'Parse error'
]
