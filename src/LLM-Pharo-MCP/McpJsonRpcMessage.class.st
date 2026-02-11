Class {
	#name : 'McpJsonRpcMessage',
	#superclass : 'Object',
	#instVars : [
		'jsonrpc'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'instance creation' }
McpJsonRpcMessage class >> fromDictionary: aDictionary [
	"Parse a JSON-RPC message from a dictionary.
	 Returns a request, response, notification, or error as appropriate."

	| hasId hasMethod hasResult hasError |
	hasId := aDictionary includesKey: 'id'.
	hasMethod := aDictionary includesKey: 'method'.
	hasResult := aDictionary includesKey: 'result'.
	hasError := aDictionary includesKey: 'error'.

	"Notification: method present, no id"
	(hasMethod and: [ hasId not ]) ifTrue: [
		^ McpJsonRpcNotification new
			  method: (aDictionary at: 'method');
			  params: (aDictionary at: 'params' ifAbsent: [ nil ]);
			  yourself ].

	"Request: method + id"
	(hasMethod and: [ hasId ]) ifTrue: [
		^ McpJsonRpcRequest new
			  id: (aDictionary at: 'id');
			  method: (aDictionary at: 'method');
			  params: (aDictionary at: 'params' ifAbsent: [ nil ]);
			  yourself ].

	"Error response"
	(hasError and: [ hasId ]) ifTrue: [
		| errDict |
		errDict := aDictionary at: 'error'.
		^ McpJsonRpcResponse new
			  id: (aDictionary at: 'id');
			  error: (McpJsonRpcError new
					   code: (errDict at: 'code');
					   message: (errDict at: 'message');
					   data: (errDict at: 'data' ifAbsent: [ nil ]);
					   yourself);
			  yourself ].

	"Success response"
	(hasResult and: [ hasId ]) ifTrue: [
		^ McpJsonRpcResponse new
			  id: (aDictionary at: 'id');
			  result: (aDictionary at: 'result');
			  yourself ].

	^ self error: 'Cannot parse JSON-RPC message'
]

{ #category : 'instance creation' }
McpJsonRpcMessage class >> fromJsonString: aString [
	"Parse a JSON-RPC message from a JSON string"

	^ self fromDictionary: (STONJSON fromString: aString)
]

{ #category : 'initialization' }
McpJsonRpcMessage >> initialize [

	super initialize.
	jsonrpc := '2.0'
]

{ #category : 'accessing' }
McpJsonRpcMessage >> jsonrpc [

	^ jsonrpc
]

{ #category : 'converting' }
McpJsonRpcMessage >> asDictionary [

	^ self subclassResponsibility
]

{ #category : 'converting' }
McpJsonRpcMessage >> asJsonString [

	^ STONJSON toString: self asDictionary
]
