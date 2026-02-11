Class {
	#name : 'McpJsonRpcError',
	#superclass : 'MCPJSONRPCMessage',
	#instVars : [
		'code',
		'message',
		'data'
	],
	#category : 'LLM-Pharo-MCP-JSON',
	#package : 'LLM-Pharo-MCP',
	#tag : 'JSON'
}

{ #category : 'instance creation' }
McpJsonRpcError class >> code: aCode message: aMessage [

	^ self new
		  code: aCode;
		  message: aMessage;
		  yourself
]

{ #category : 'instance creation' }
McpJsonRpcError class >> code: aCode message: aMessage data: someData [

	^ self new
		  code: aCode;
		  message: aMessage;
		  data: someData;
		  yourself
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> internalError [

	^ self
		  code: MCPConstants errorCodeInternalError
		  message: 'Internal error'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> internalError: aMessage [

	^ self
		  code: MCPConstants errorCodeInternalError
		  message: aMessage
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> invalidParams [

	^ self
		  code: MCPConstants errorCodeInvalidParams
		  message: 'Invalid params'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> invalidParams: aMessage [

	^ self
		  code: MCPConstants errorCodeInvalidParams
		  message: aMessage
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> invalidRequest [

	^ self
		  code: MCPConstants errorCodeInvalidRequest
		  message: 'Invalid request'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> methodNotFound [

	^ self
		  code: MCPConstants errorCodeMethodNotFound
		  message: 'Method not found'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> methodNotFound: aMethodName [

	^ self
		  code: MCPConstants errorCodeMethodNotFound
		  message: 'Method not found: ' , aMethodName
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> parseError [

	^ self code: MCPConstants errorCodeParseError message: 'Parse error'
]

{ #category : 'converting' }
McpJsonRpcError >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'code' put: code.
	dict at: 'message' put: message.
	data ifNotNil: [ dict at: 'data' put: data ].
	^ dict
]

{ #category : 'accessing' }
McpJsonRpcError >> code [

	^ code
]

{ #category : 'accessing' }
McpJsonRpcError >> code: anObject [

	code := anObject
]

{ #category : 'accessing' }
McpJsonRpcError >> data [

	^ data
]

{ #category : 'accessing' }
McpJsonRpcError >> data: anObject [

	data := anObject
]

{ #category : 'accessing' }
McpJsonRpcError >> message [

	^ message
]

{ #category : 'accessing' }
McpJsonRpcError >> message: anObject [

	message := anObject
]
