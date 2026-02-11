Class {
	#name : 'McpJsonRpcNotification',
	#superclass : 'McpJsonRpcMessage',
	#instVars : [
		'method',
		'params'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'instance creation' }
McpJsonRpcNotification class >> method: aMethod [

	^ self new
		  method: aMethod;
		  yourself
]

{ #category : 'instance creation' }
McpJsonRpcNotification class >> method: aMethod params: aDict [

	^ self new
		  method: aMethod;
		  params: aDict;
		  yourself
]

{ #category : 'accessing' }
McpJsonRpcNotification >> method [

	^ method
]

{ #category : 'accessing' }
McpJsonRpcNotification >> method: anObject [

	method := anObject
]

{ #category : 'accessing' }
McpJsonRpcNotification >> params [

	^ params
]

{ #category : 'accessing' }
McpJsonRpcNotification >> params: anObject [

	params := anObject
]

{ #category : 'testing' }
McpJsonRpcNotification >> isNotification [

	^ true
]

{ #category : 'converting' }
McpJsonRpcNotification >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'jsonrpc' put: self jsonrpc.
	dict at: 'method' put: method.
	params ifNotNil: [ dict at: 'params' put: params ].
	^ dict
]
