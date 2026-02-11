Class {
	#name : 'McpJsonRpcNotification',
	#superclass : 'MCPJSONRPCMessage',
	#instVars : [
		'method',
		'params'
	],
	#category : 'LLM-Pharo-MCP-JSON',
	#package : 'LLM-Pharo-MCP',
	#tag : 'JSON'
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
