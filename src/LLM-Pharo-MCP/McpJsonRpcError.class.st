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
