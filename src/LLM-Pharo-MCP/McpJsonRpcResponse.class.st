Class {
	#name : 'McpJsonRpcResponse',
	#superclass : 'McpJsonRpcMessage',
	#instVars : [
		'id',
		'result',
		'error'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'instance creation' }
McpJsonRpcResponse class >> id: anId result: aResult [

	^ self new
		  id: anId;
		  result: aResult;
		  yourself
]

{ #category : 'instance creation' }
McpJsonRpcResponse class >> id: anId error: anError [

	^ self new
		  id: anId;
		  error: anError;
		  yourself
]

{ #category : 'accessing' }
McpJsonRpcResponse >> id [

	^ id
]

{ #category : 'accessing' }
McpJsonRpcResponse >> id: anObject [

	id := anObject
]

{ #category : 'accessing' }
McpJsonRpcResponse >> result [

	^ result
]

{ #category : 'accessing' }
McpJsonRpcResponse >> result: anObject [

	result := anObject
]

{ #category : 'accessing' }
McpJsonRpcResponse >> error [

	^ error
]

{ #category : 'accessing' }
McpJsonRpcResponse >> error: anObject [

	error := anObject
]

{ #category : 'testing' }
McpJsonRpcResponse >> isSuccess [

	^ error isNil
]

{ #category : 'testing' }
McpJsonRpcResponse >> isError [

	^ error isNotNil
]

{ #category : 'converting' }
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
