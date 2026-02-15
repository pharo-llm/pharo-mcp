Class {
	#name : 'McpInMemoryTransport',
	#superclass : 'MCPTransport',
	#instVars : [
		'sentMessages',
		'running'
	],
	#category : 'LLM-Pharo-MCP-Transport',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Transport'
}

{ #category : 'initialization' }
McpInMemoryTransport >> initialize [

	super initialize.
	sentMessages := OrderedCollection new.
	running := false
]

{ #category : 'initialization' }
McpInMemoryTransport >> isRunning [

	^ running
]

{ #category : 'initialization' }
McpInMemoryTransport >> lastSentMessage [

	^ sentMessages ifEmpty: [ nil ] ifNotEmpty: [ sentMessages last ]
]

{ #category : 'initialization' }
McpInMemoryTransport >> lastSentParsed [

	| last |
	last := self lastSentMessage.
	last ifNil: [ ^ nil ].
	^ self normalizeParsedObject: (STONJSON fromString: last)
]

{ #category : 'initialization' }
McpInMemoryTransport >> normalizeParsedObject: anObject [

	(anObject isKindOf: Dictionary) ifTrue: [
		| ordered |
		ordered := OrderedDictionary new.
		anObject keysAndValuesDo: [ :key :value |
			ordered at: key put: (self normalizeParsedObject: value) ].
		^ ordered ].
	(anObject isArray) ifTrue: [
		^ anObject collect: [ :each | self normalizeParsedObject: each ] ].
	^ anObject
]

{ #category : 'initialization' }
McpInMemoryTransport >> reset [

	sentMessages removeAll
]

{ #category : 'operations' }
McpInMemoryTransport >> sendMessage: aJsonString [

	sentMessages add: aJsonString
]

{ #category : 'initialization' }
McpInMemoryTransport >> sentMessages [

	^ sentMessages
]

{ #category : 'operations' }
McpInMemoryTransport >> simulateReceive: aJsonString [
	"Simulate receiving a message from a client.
	 This triggers the message handler as if the message arrived via transport."

	messageHandler ifNotNil: [ messageHandler value: aJsonString ]
]

{ #category : 'initialization' }
McpInMemoryTransport >> start [

	running := true
]

{ #category : 'initialization' }
McpInMemoryTransport >> stop [

	running := false
]
