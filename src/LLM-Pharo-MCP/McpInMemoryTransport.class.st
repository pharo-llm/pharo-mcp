Class {
	#name : 'McpInMemoryTransport',
	#superclass : 'McpTransport',
	#instVars : [
		'sentMessages',
		'running'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'initialization' }
McpInMemoryTransport >> initialize [

	super initialize.
	sentMessages := OrderedCollection new.
	running := false
]

{ #category : 'accessing' }
McpInMemoryTransport >> sentMessages [

	^ sentMessages
]

{ #category : 'accessing' }
McpInMemoryTransport >> lastSentMessage [

	^ sentMessages ifEmpty: [ nil ] ifNotEmpty: [ sentMessages last ]
]

{ #category : 'accessing' }
McpInMemoryTransport >> lastSentParsed [

	| last |
	last := self lastSentMessage.
	last ifNil: [ ^ nil ].
	^ STONJSON fromString: last
]

{ #category : 'testing' }
McpInMemoryTransport >> isRunning [

	^ running
]

{ #category : 'operations' }
McpInMemoryTransport >> start [

	running := true
]

{ #category : 'operations' }
McpInMemoryTransport >> stop [

	running := false
]

{ #category : 'operations' }
McpInMemoryTransport >> sendMessage: aJsonString [

	sentMessages add: aJsonString
]

{ #category : 'operations' }
McpInMemoryTransport >> simulateReceive: aJsonString [
	"Simulate receiving a message from a client.
	 This triggers the message handler as if the message arrived via transport."

	messageHandler ifNotNil: [ messageHandler value: aJsonString ]
]

{ #category : 'operations' }
McpInMemoryTransport >> reset [

	sentMessages removeAll
]
