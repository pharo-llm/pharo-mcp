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
