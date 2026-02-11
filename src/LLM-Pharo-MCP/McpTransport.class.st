Class {
	#name : 'McpTransport',
	#superclass : 'Object',
	#instVars : [
		'messageHandler'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'accessing' }
McpTransport >> messageHandler [

	^ messageHandler
]

{ #category : 'accessing' }
McpTransport >> messageHandler: anObject [

	messageHandler := anObject
]

{ #category : 'operations' }
McpTransport >> start [

	self subclassResponsibility
]

{ #category : 'operations' }
McpTransport >> stop [

	self subclassResponsibility
]

{ #category : 'operations' }
McpTransport >> sendMessage: aJsonString [

	self subclassResponsibility
]
