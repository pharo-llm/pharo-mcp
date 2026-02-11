Class {
	#name : 'McpJsonRpcResponse',
	#superclass : 'MCPJSONRPCMessage',
	#instVars : [
		'id',
		'result',
		'error'
	],
	#category : 'LLM-Pharo-MCP-JSON',
	#package : 'LLM-Pharo-MCP',
	#tag : 'JSON'
}

{ #category : 'instance creation' }
McpJsonRpcResponse class >> id: anId error: anError [

	^ self new
		  id: anId;
		  error: anError;
		  yourself
]

{ #category : 'instance creation' }
McpJsonRpcResponse class >> id: anId result: aResult [

	^ self new
		  id: anId;
		  result: aResult;
		  yourself
]

{ #category : 'initialization' }
McpJsonRpcResponse >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'jsonrpc' put: self jsonrpc.
	dict at: 'id' put: id.
	self isError
		ifTrue: [ dict at: 'error' put: error asDictionary ]
		ifFalse: [ dict at: 'result' put: (result ifNil: [ Dictionary new ]) ].
	^ dict
]

{ #category : 'accessing' }
McpJsonRpcResponse >> error [

	^ error
]

{ #category : 'error handling' }
McpJsonRpcResponse >> error: anObject [

	error := anObject
]

{ #category : 'accessing' }
McpJsonRpcResponse >> id [

	^ id
]

{ #category : 'accessing' }
McpJsonRpcResponse >> id: anObject [

	id := anObject
]

{ #category : 'testing' }
McpJsonRpcResponse >> isError [

	^ error isNotNil
]

{ #category : 'testing' }
McpJsonRpcResponse >> isSuccess [

	^ error isNil
]

{ #category : 'accessing' }
McpJsonRpcResponse >> result [

	^ result
]

{ #category : 'accessing' }
McpJsonRpcResponse >> result: anObject [

	result := anObject
]
