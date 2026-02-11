Class {
	#name : 'McpJsonRpcRequest',
	#superclass : 'MCPJSONRPCMessage',
	#instVars : [
		'id',
		'method',
		'params'
	],
	#category : 'LLM-Pharo-MCP-JSON',
	#package : 'LLM-Pharo-MCP',
	#tag : 'JSON'
}

{ #category : 'instance creation' }
McpJsonRpcRequest class >> id: anId method: aMethod [

	^ self new
		  id: anId;
		  method: aMethod;
		  yourself
]

{ #category : 'as yet unclassified' }
McpJsonRpcRequest class >> id: anId method: aMethod params: aDict [

	^ self new
		  id: anId;
		  method: aMethod;
		  params: aDict;
		  yourself
]

{ #category : 'converting' }
McpJsonRpcRequest >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'jsonrpc' put: self jsonrpc.
	dict at: 'id' put: id.
	dict at: 'method' put: method.
	params ifNotNil: [ dict at: 'params' put: params ].
	^ dict
]

{ #category : 'accessing' }
McpJsonRpcRequest >> id [

	^ id
]

{ #category : 'accessing' }
McpJsonRpcRequest >> id: anObject [

	id := anObject
]

{ #category : 'testing' }
McpJsonRpcRequest >> isRequest [

	^ true
]

{ #category : 'accessing' }
McpJsonRpcRequest >> method [

	^ method
]

{ #category : 'accessing' }
McpJsonRpcRequest >> method: anObject [

	method := anObject
]

{ #category : 'accessing' }
McpJsonRpcRequest >> params [

	^ params
]

{ #category : 'accessing' }
McpJsonRpcRequest >> params: anObject [

	params := anObject
]
