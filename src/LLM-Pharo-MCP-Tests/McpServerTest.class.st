Class {
	#name : 'McpServerTest',
	#superclass : 'TestCase',
	#instVars : [
		'server',
		'transport'
	],
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'running' }
McpServerTest >> sendNotification: method [

	| notification |
	notification := McpJsonRpcNotification method: method.
	transport simulateReceive: notification asJsonString
]

{ #category : 'running' }
McpServerTest >> sendRequest: method id: anId [

	^ self sendRequest: method id: anId params: nil
]

{ #category : 'running' }
McpServerTest >> sendRequest: method id: anId params: params [

	| request |
	request := McpJsonRpcRequest id: anId method: method params: params.
	transport simulateReceive: request asJsonString.
	^ transport lastSentParsed
]

{ #category : 'running' }
McpServerTest >> setUp [

	super setUp.
	server := McpServer name: 'test-server' version: '1.0.0'.
	transport := McpInMemoryTransport new.
	server transport: transport.
	server start
]

{ #category : 'running' }
McpServerTest >> tearDown [

	server stop.
	super tearDown

]

{ #category : 'running' }
McpServerTest >> testAddTool [

	| tool |
	tool := McpTool name: 'echo' description: 'Echo' handler: [ :a | McpToolResult successText: 'ok' ].
	server addTool: tool.
	self assert: server tools size equals: 1.
	self assert: (server toolNamed: 'echo') equals: tool
]

{ #category : 'running' }
McpServerTest >> testDefaultServer [

	| s |
	s := McpServer default.
	self assert: s name equals: MCPConstants mcpServerName.
	self assert: s version equals: MCPConstants mcpServerVersion
]

{ #category : 'running' }
McpServerTest >> testHandleInitialize [

	| response result |
	response := self sendRequest: 'initialize' id: 1.
	result := response at: 'result'.
	self assert: (result at: 'protocolVersion') equals: MCPConstants mcpProtocolVersion.
	self assert: ((result at: 'serverInfo') at: 'name') equals: 'test-server'.
	self assert: ((result at: 'serverInfo') at: 'version') equals: '1.0.0'.
	self assert: (result includesKey: 'capabilities')
]

{ #category : 'running' }
McpServerTest >> testHandleInitializeCapabilities [

	| response caps |
	response := self sendRequest: 'initialize' id: 1.
	caps := (response at: 'result') at: 'capabilities'.
	self assert: (caps includesKey: 'tools').
	self assert: (caps includesKey: 'resources').
	self assert: (caps includesKey: 'prompts')
]

{ #category : 'running' }
McpServerTest >> testHandlePing [

	| response |
	response := self sendRequest: 'ping' id: 99.
	self assert: (response at: 'id') equals: 99.
	self assert: (response at: 'result') equals: OrderedDictionary new
]

{ #category : 'running' }
McpServerTest >> testInitializedNotification [

	self deny: server isInitialized.
	self sendNotification: 'notifications/initialized'.
	self assert: server isInitialized
]

{ #category : 'running' }
McpServerTest >> testRemoveNonexistentTool [

	"Should not raise error"
	server removeTool: 'nonexistent'.
	self assert: server tools isEmpty
]

{ #category : 'running' }
McpServerTest >> testRemoveTool [

	| tool |
	tool := McpTool name: 'echo' description: 'Echo' handler: [ :a | McpToolResult successText: 'ok' ].
	server addTool: tool.
	server removeTool: 'echo'.
	self assert: server tools isEmpty.
	self assert: (server toolNamed: 'echo') isNil
]

{ #category : 'running' }
McpServerTest >> testServerInitialize [

	| s |
	s := McpServer new.
	self assert: s tools isEmpty.
	self assert: s resources isEmpty.
	self assert: s resourceTemplates isEmpty.
	self assert: s prompts isEmpty.
	self deny: s isInitialized
]

{ #category : 'running' }
McpServerTest >> testStartWithoutTransportRaisesError [

	| s |
	s := McpServer new.
	self should: [ s start ] raise: Error
]

{ #category : 'running' }
McpServerTest >> testToolsCall [

	| response result |
	server addTool: (McpTool
			 name: 'echo'
			 description: 'Echoes input'
			 handler: [ :args | McpToolResult successText: (args at: 'message') ]).
	response := self sendRequest: 'tools/call' id: 10 params: (Dictionary new
				         at: 'name' put: 'echo';
				         at: 'arguments' put: (Dictionary new at: 'message' put: 'hello'; yourself);
				         yourself).
	result := response at: 'result'.
	self assert: ((result at: 'content') first at: 'text') equals: 'hello'.
	self deny: (result includesKey: 'isError')
]

{ #category : 'running' }
McpServerTest >> testToolsCallMissingParams [

	| response error |
	response := self sendRequest: 'tools/call' id: 13.
	error := response at: 'error'.
	self assert: (error at: 'code') equals: MCPConstants errorCodeInvalidParams
]

{ #category : 'running' }
McpServerTest >> testToolsCallMissingToolName [

	| response error |
	response := self sendRequest: 'tools/call' id: 14 params: (Dictionary new
				         at: 'arguments' put: Dictionary new;
				         yourself).
	error := response at: 'error'.
	self assert: (error at: 'code') equals: MCPConstants errorCodeInvalidParams
]

{ #category : 'running' }
McpServerTest >> testToolsCallUnknownTool [

	| response error |
	response := self sendRequest: 'tools/call' id: 12 params: (Dictionary new
				         at: 'name' put: 'nonexistent';
				         at: 'arguments' put: Dictionary new;
				         yourself).
	error := response at: 'error'.
	self assert: (error at: 'code') equals: MCPConstants errorCodeInvalidParams.
	self assert: ((error at: 'message') includesSubstring: 'nonexistent')
]

{ #category : 'running' }
McpServerTest >> testToolsCallWithError [

	| response result |
	server addTool: (McpTool
			 name: 'fail'
			 description: 'Fails'
			 handler: [ :args | self error: 'Boom' ]).
	response := self sendRequest: 'tools/call' id: 11 params: (Dictionary new
				         at: 'name' put: 'fail';
				         at: 'arguments' put: Dictionary new;
				         yourself).
	result := response at: 'result'.
	self assert: (result at: 'isError') equals: true.
	self assert: ((result at: 'content') first at: 'text' ) equals: 'Boom'
]

{ #category : 'running' }
McpServerTest >> testToolsList [

	| response result |
	server addTool: (McpTool
			 name: 'echo'
			 description: 'Echoes input'
			 inputSchema: (McpSchema object: { 'msg' -> McpSchema string } required: #( 'msg' ))
			 handler: [ :a | McpToolResult successText: (a at: 'msg') ]).
	server addTool: (McpTool
			 name: 'ping'
			 description: 'Ping tool'
			 handler: [ :a | McpToolResult successText: 'pong' ]).
	response := self sendRequest: 'tools/list' id: 2.
	result := response at: 'result'.
	self assert: (result at: 'tools') size equals: 2
]

{ #category : 'running' }
McpServerTest >> testToolsListContent [

	| response toolDict |
	server addTool: (McpTool
			 name: 'add'
			 description: 'Adds numbers'
			 inputSchema: (McpSchema
					  object: { 'a' -> McpSchema number. 'b' -> McpSchema number }
					  required: #( 'a' 'b' ))
			 handler: [ :a | McpToolResult successText: 'done' ]).
	response := self sendRequest: 'tools/list' id: 4.
	toolDict := (response at: 'result') at: 'tools'.
	self assert: toolDict first isNotNil.
	self assert: (toolDict first at: 'name') equals: 'add'.
	self assert: (toolDict first at: 'description') equals: 'Adds numbers'.
	self assert: ((toolDict first at: 'inputSchema') at: 'type') equals: 'object'
]

{ #category : 'running' }
McpServerTest >> testToolsListEmpty [

	| response result |
	response := self sendRequest: 'tools/list' id: 3.
	result := response at: 'result'.
	self assert: (result at: 'tools') isEmpty
]

{ #category : 'running' }
McpServerTest >> testUnknownMethod [

	| response error |
	response := self sendRequest: 'unknown/method' id: 5.
	error := response at: 'error'.
	self assert: (error at: 'code') equals: MCPConstants errorCodeMethodNotFound.
	self assert: ((error at: 'message') includesSubstring: 'unknown/method')
]
