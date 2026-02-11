Class {
	#name : 'McpInMemoryTransportTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpInMemoryTransportTest >> testInitialization [

	| transport |
	transport := McpInMemoryTransport new.
	self assert: transport sentMessages isEmpty.
	self deny: transport isRunning
]

{ #category : 'tests' }
McpInMemoryTransportTest >> testLastSentMessageWhenEmpty [

	| transport |
	transport := McpInMemoryTransport new.
	self assert: transport lastSentMessage isNil.
	self assert: transport lastSentParsed isNil
]

{ #category : 'tests' }
McpInMemoryTransportTest >> testLastSentParsed [

	| transport parsed |
	transport := McpInMemoryTransport new.
	transport sendMessage: '{"jsonrpc":"2.0","id":1,"result":{"status":"ok"}}'.
	parsed := transport lastSentParsed.
	self assert: (parsed at: 'jsonrpc') equals: '2.0'.
	self assert: ((parsed at: 'result') at: 'status') equals: 'ok'
]

{ #category : 'tests' }
McpInMemoryTransportTest >> testReset [

	| transport |
	transport := McpInMemoryTransport new.
	transport sendMessage: 'msg1'.
	transport sendMessage: 'msg2'.
	self assert: transport sentMessages size equals: 2.
	transport reset.
	self assert: transport sentMessages isEmpty
]

{ #category : 'tests' }
McpInMemoryTransportTest >> testSendMessage [

	| transport |
	transport := McpInMemoryTransport new.
	transport sendMessage: '{"jsonrpc":"2.0","id":1,"method":"ping"}'.
	self assert: transport sentMessages size equals: 1.
	self assert: (transport lastSentMessage includesSubstring: 'ping')
]

{ #category : 'tests' }
McpInMemoryTransportTest >> testSendMultipleMessages [

	| transport |
	transport := McpInMemoryTransport new.
	transport sendMessage: 'message1'.
	transport sendMessage: 'message2'.
	transport sendMessage: 'message3'.
	self assert: transport sentMessages size equals: 3.
	self assert: transport lastSentMessage equals: 'message3'
]

{ #category : 'tests' }
McpInMemoryTransportTest >> testSimulateReceive [

	| transport received |
	received := OrderedCollection new.
	transport := McpInMemoryTransport new.
	transport messageHandler: [ :msg | received add: msg ].
	transport simulateReceive: 'test message'.
	self assert: received size equals: 1.
	self assert: received first equals: 'test message'
]

{ #category : 'tests' }
McpInMemoryTransportTest >> testSimulateReceiveWithoutHandler [

	| transport |
	transport := McpInMemoryTransport new.
	"Should not raise an error"
	transport simulateReceive: 'test message'
]

{ #category : 'tests' }
McpInMemoryTransportTest >> testStartAndStop [

	| transport |
	transport := McpInMemoryTransport new.
	transport start.
	self assert: transport isRunning.
	transport stop.
	self deny: transport isRunning
]
