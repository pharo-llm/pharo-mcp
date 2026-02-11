Class {
	#name : 'McpJsonRpcError',
	#superclass : 'Object',
	#instVars : [
		'code',
		'message',
		'data'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
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
McpJsonRpcError class >> parseError [

	^ self code: McpConstants errorCodeParseError message: 'Parse error'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> invalidRequest [

	^ self
		  code: McpConstants errorCodeInvalidRequest
		  message: 'Invalid request'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> methodNotFound [

	^ self
		  code: McpConstants errorCodeMethodNotFound
		  message: 'Method not found'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> methodNotFound: aMethodName [

	^ self
		  code: McpConstants errorCodeMethodNotFound
		  message: 'Method not found: ' , aMethodName
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> invalidParams [

	^ self
		  code: McpConstants errorCodeInvalidParams
		  message: 'Invalid params'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> invalidParams: aMessage [

	^ self
		  code: McpConstants errorCodeInvalidParams
		  message: aMessage
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> internalError [

	^ self
		  code: McpConstants errorCodeInternalError
		  message: 'Internal error'
]

{ #category : 'predefined errors' }
McpJsonRpcError class >> internalError: aMessage [

	^ self
		  code: McpConstants errorCodeInternalError
		  message: aMessage
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
McpJsonRpcError >> message [

	^ message
]

{ #category : 'accessing' }
McpJsonRpcError >> message: anObject [

	message := anObject
]

{ #category : 'accessing' }
McpJsonRpcError >> data [

	^ data
]

{ #category : 'accessing' }
McpJsonRpcError >> data: anObject [

	data := anObject
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
