"
I define the transport API used by the MCP server.

"
Class {
	#name : 'MCPTransport',
	#superclass : 'Object',
	#instVars : [
		'messageHandler',
		'running'
	],
	#category : 'LLM-Pharo-MCP-Transport',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Transport'
}

{ #category : 'as yet unclassified' }
MCPTransport >> isRunning [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
MCPTransport >> messageHandler [

	^ messageHandler
]

{ #category : 'accessing' }
MCPTransport >> messageHandler: anObject [

	messageHandler := anObject
]

{ #category : 'operations' }
MCPTransport >> sendMessage: aJsonString [

	self subclassResponsibility
]

{ #category : 'operations' }
MCPTransport >> start [

	self subclassResponsibility
]

{ #category : 'operations' }
MCPTransport >> stop [

	self subclassResponsibility
]
