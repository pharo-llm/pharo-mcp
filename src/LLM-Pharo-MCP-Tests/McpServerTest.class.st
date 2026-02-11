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

{ #category : 'helpers' }
McpServerTest >> sendRequest: method id: anId [

	^ self sendRequest: method id: anId params: nil
]

{ #category : 'helpers' }
McpServerTest >> sendRequest: method id: anId params: params [

	| request |
	request := McpJsonRpcRequest id: anId method: method params: params.
	transport simulateReceive: request asJsonString.
	^ transport lastSentParsed
]

{ #category : 'helpers' }
McpServerTest >> sendNotification: method [

	| notification |
	notification := McpJsonRpcNotification method: method.
	transport simulateReceive: notification asJsonString
]

{ #category : 'tests - initialization' }
McpServerTest >> testDefaultServer [

	| s |
	s := McpServer default.
	self assert: s name equals: McpConstants mcpServerName.
	self assert: s version equals: McpConstants mcpServerVersion
]

{ #category : 'tests - initialization' }
McpServerTest >> testServerInitialize [

	| s |
	s := McpServer new.
	self assert: s tools isEmpty.
	self assert: s resources isEmpty.
	self assert: s resourceTemplates isEmpty.
	self assert: s prompts isEmpty.
	self deny: s isInitialized
]

{ #category : 'tests - initialization' }
McpServerTest >> testStartWithoutTransportRaisesError [

	| s |
	s := McpServer new.
	self should: [ s start ] raise: Error
]

{ #category : 'tests - protocol - initialize' }
McpServerTest >> testHandleInitialize [

	| response result |
	response := self sendRequest: 'initialize' id: 1.
	result := response at: 'result'.
	self assert: (result at: 'protocolVersion') equals: McpConstants mcpProtocolVersion.
	self assert: ((result at: 'serverInfo') at: 'name') equals: 'test-server'.
	self assert: ((result at: 'serverInfo') at: 'version') equals: '1.0.0'.
	self assert: (result includesKey: 'capabilities')
]

{ #category : 'tests - protocol - initialize' }
McpServerTest >> testHandleInitializeCapabilities [

	| response caps |
	response := self sendRequest: 'initialize' id: 1.
	caps := (response at: 'result') at: 'capabilities'.
	self assert: (caps includesKey: 'tools').
	self assert: (caps includesKey: 'resources').
	self assert: (caps includesKey: 'prompts')
]

{ #category : 'tests - protocol - ping' }
McpServerTest >> testHandlePing [

	| response |
	response := self sendRequest: 'ping' id: 99.
	self assert: (response at: 'id') equals: 99.
	self assert: (response at: 'result') equals: OrderedDictionary new
]

{ #category : 'tests - protocol - notifications' }
McpServerTest >> testInitializedNotification [

	self deny: server isInitialized.
	self sendNotification: 'notifications/initialized'.
	self assert: server isInitialized
]

{ #category : 'tests - protocol - method not found' }
McpServerTest >> testUnknownMethod [

	| response error |
	response := self sendRequest: 'unknown/method' id: 5.
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeMethodNotFound.
	self assert: ((error at: 'message') includesSubstring: 'unknown/method')
]

{ #category : 'tests - tools management' }
McpServerTest >> testAddTool [

	| tool |
	tool := McpTool name: 'echo' description: 'Echo' handler: [ :a | McpToolResult successText: 'ok' ].
	server addTool: tool.
	self assert: server tools size equals: 1.
	self assert: (server toolNamed: 'echo') equals: tool
]

{ #category : 'tests - tools management' }
McpServerTest >> testRemoveTool [

	| tool |
	tool := McpTool name: 'echo' description: 'Echo' handler: [ :a | McpToolResult successText: 'ok' ].
	server addTool: tool.
	server removeTool: 'echo'.
	self assert: server tools isEmpty.
	self assert: (server toolNamed: 'echo') isNil
]

{ #category : 'tests - tools management' }
McpServerTest >> testRemoveNonexistentTool [

	"Should not raise error"
	server removeTool: 'nonexistent'.
	self assert: server tools isEmpty
]

{ #category : 'tests - protocol - tools/list' }
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

{ #category : 'tests - protocol - tools/list' }
McpServerTest >> testToolsListEmpty [

	| response result |
	response := self sendRequest: 'tools/list' id: 3.
	result := response at: 'result'.
	self assert: (result at: 'tools') isEmpty
]

{ #category : 'tests - protocol - tools/list' }
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

{ #category : 'tests - protocol - tools/call' }
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

{ #category : 'tests - protocol - tools/call' }
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

{ #category : 'tests - protocol - tools/call' }
McpServerTest >> testToolsCallUnknownTool [

	| response error |
	response := self sendRequest: 'tools/call' id: 12 params: (Dictionary new
				         at: 'name' put: 'nonexistent';
				         at: 'arguments' put: Dictionary new;
				         yourself).
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams.
	self assert: ((error at: 'message') includesSubstring: 'nonexistent')
]

{ #category : 'tests - protocol - tools/call' }
McpServerTest >> testToolsCallMissingParams [

	| response error |
	response := self sendRequest: 'tools/call' id: 13.
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams
]

{ #category : 'tests - protocol - tools/call' }
McpServerTest >> testToolsCallMissingToolName [

	| response error |
	response := self sendRequest: 'tools/call' id: 14 params: (Dictionary new
				         at: 'arguments' put: Dictionary new;
				         yourself).
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams
]

{ #category : 'tests - protocol - tools/call' }
McpServerTest >> testToolsCallWithoutArguments [

	| response result |
	server addTool: (McpTool
			 name: 'noargs'
			 description: 'No args needed'
			 handler: [ :args | McpToolResult successText: 'done' ]).
	response := self sendRequest: 'tools/call' id: 15 params: (Dictionary new
				         at: 'name' put: 'noargs';
				         yourself).
	result := response at: 'result'.
	self assert: ((result at: 'content') first at: 'text') equals: 'done'
]

{ #category : 'tests - resources management' }
McpServerTest >> testAddResource [

	| resource |
	resource := McpResource
		            uri: 'pharo://info'
		            name: 'Info'
		            description: 'System info'
		            handler: [ { McpContentItem text: 'info' } ].
	server addResource: resource.
	self assert: server resources size equals: 1.
	self assert: (server resourceAtUri: 'pharo://info') equals: resource
]

{ #category : 'tests - resources management' }
McpServerTest >> testRemoveResource [

	server addResource: (McpResource
			 uri: 'pharo://info'
			 name: 'Info'
			 description: 'System info'
			 handler: [ { McpContentItem text: 'info' } ]).
	server removeResource: 'pharo://info'.
	self assert: server resources isEmpty
]

{ #category : 'tests - protocol - resources/list' }
McpServerTest >> testResourcesList [

	| response result |
	server addResource: (McpResource
			 uri: 'pharo://info'
			 name: 'Info'
			 description: 'System info'
			 handler: [ { McpContentItem text: 'info' } ]).
	server addResource: (McpResource
			 uri: 'pharo://version'
			 name: 'Version'
			 description: 'Pharo version'
			 handler: [ { McpContentItem text: '13' } ]).
	response := self sendRequest: 'resources/list' id: 20.
	result := response at: 'result'.
	self assert: (result at: 'resources') size equals: 2
]

{ #category : 'tests - protocol - resources/list' }
McpServerTest >> testResourcesListEmpty [

	| response |
	response := self sendRequest: 'resources/list' id: 21.
	self assert: ((response at: 'result') at: 'resources') isEmpty
]

{ #category : 'tests - protocol - resources/read' }
McpServerTest >> testResourcesRead [

	| response contents |
	server addResource: (McpResource
			 uri: 'pharo://version'
			 name: 'Version'
			 description: 'Pharo version'
			 handler: [ { McpContentItem text: 'Pharo 13.0' } ]).
	response := self sendRequest: 'resources/read' id: 22 params: (Dictionary new
				         at: 'uri' put: 'pharo://version';
				         yourself).
	contents := (response at: 'result') at: 'contents'.
	self assert: contents size equals: 1.
	self assert: (contents first at: 'text') equals: 'Pharo 13.0'
]

{ #category : 'tests - protocol - resources/read' }
McpServerTest >> testResourcesReadNotFound [

	| response error |
	response := self sendRequest: 'resources/read' id: 23 params: (Dictionary new
				         at: 'uri' put: 'pharo://nonexistent';
				         yourself).
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams.
	self assert: ((error at: 'message') includesSubstring: 'nonexistent')
]

{ #category : 'tests - protocol - resources/read' }
McpServerTest >> testResourcesReadMissingUri [

	| response error |
	response := self sendRequest: 'resources/read' id: 24 params: Dictionary new.
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams
]

{ #category : 'tests - protocol - resources/read' }
McpServerTest >> testResourcesReadMissingParams [

	| response error |
	response := self sendRequest: 'resources/read' id: 25.
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams
]

{ #category : 'tests - protocol - resources/read with templates' }
McpServerTest >> testResourcesReadViaTemplate [

	| response contents |
	server addResourceTemplate: (McpResourceTemplate
			 uriTemplate: 'pharo://class/{className}'
			 name: 'Class'
			 description: 'Class details'
			 handler: [ :params |
				 { McpContentItem text: 'Class: ' , (params at: 'className') } ]).
	response := self sendRequest: 'resources/read' id: 26 params: (Dictionary new
				         at: 'uri' put: 'pharo://class/SmallInteger';
				         yourself).
	contents := (response at: 'result') at: 'contents'.
	self assert: contents size equals: 1.
	self assert: (contents first at: 'text') equals: 'Class: SmallInteger'
]

{ #category : 'tests - protocol - resources/templates/list' }
McpServerTest >> testResourcesTemplatesList [

	| response result |
	server addResourceTemplate: (McpResourceTemplate
			 uriTemplate: 'pharo://class/{className}'
			 name: 'Class'
			 description: 'Class details'
			 handler: [ :p | {  } ]).
	response := self sendRequest: 'resources/templates/list' id: 27.
	result := response at: 'result'.
	self assert: (result at: 'resourceTemplates') size equals: 1.
	self
		assert: ((result at: 'resourceTemplates') first at: 'uriTemplate')
		equals: 'pharo://class/{className}'
]

{ #category : 'tests - protocol - resources/templates/list' }
McpServerTest >> testResourcesTemplatesListEmpty [

	| response |
	response := self sendRequest: 'resources/templates/list' id: 28.
	self assert: ((response at: 'result') at: 'resourceTemplates') isEmpty
]

{ #category : 'tests - prompts management' }
McpServerTest >> testAddPrompt [

	| prompt |
	prompt := McpPrompt name: 'review' description: 'Review code' handler: [ :a | Dictionary new ].
	server addPrompt: prompt.
	self assert: server prompts size equals: 1.
	self assert: (server promptNamed: 'review') equals: prompt
]

{ #category : 'tests - prompts management' }
McpServerTest >> testRemovePrompt [

	server addPrompt: (McpPrompt name: 'review' description: 'Review' handler: [ :a | Dictionary new ]).
	server removePrompt: 'review'.
	self assert: server prompts isEmpty
]

{ #category : 'tests - protocol - prompts/list' }
McpServerTest >> testPromptsList [

	| response result |
	server addPrompt: (McpPrompt
			 name: 'review'
			 description: 'Code review'
			 arguments: { McpPromptArgument required: 'code' description: 'The code' }
			 handler: [ :a | Dictionary new ]).
	response := self sendRequest: 'prompts/list' id: 30.
	result := response at: 'result'.
	self assert: (result at: 'prompts') size equals: 1.
	self assert: ((result at: 'prompts') first at: 'name') equals: 'review'
]

{ #category : 'tests - protocol - prompts/list' }
McpServerTest >> testPromptsListEmpty [

	| response |
	response := self sendRequest: 'prompts/list' id: 31.
	self assert: ((response at: 'result') at: 'prompts') isEmpty
]

{ #category : 'tests - protocol - prompts/get' }
McpServerTest >> testPromptsGet [

	| response result |
	server addPrompt: (McpPrompt
			 name: 'greet'
			 description: 'Greet user'
			 arguments: { McpPromptArgument required: 'name' description: 'User name' }
			 handler: [ :args |
				 Dictionary new
					 at: 'description' put: 'Greeting';
					 at: 'messages'
					 put: { (McpPromptMessage user: 'Hello ' , (args at: 'name')) asDictionary };
					 yourself ]).
	response := self sendRequest: 'prompts/get' id: 32 params: (Dictionary new
				         at: 'name' put: 'greet';
				         at: 'arguments' put: (Dictionary new at: 'name' put: 'Bob'; yourself);
				         yourself).
	result := response at: 'result'.
	self assert: (result at: 'description') equals: 'Greeting'.
	self assert: (result at: 'messages') size equals: 1
]

{ #category : 'tests - protocol - prompts/get' }
McpServerTest >> testPromptsGetUnknown [

	| response error |
	response := self sendRequest: 'prompts/get' id: 33 params: (Dictionary new
				         at: 'name' put: 'nonexistent';
				         yourself).
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams.
	self assert: ((error at: 'message') includesSubstring: 'nonexistent')
]

{ #category : 'tests - protocol - prompts/get' }
McpServerTest >> testPromptsGetMissingParams [

	| response error |
	response := self sendRequest: 'prompts/get' id: 34.
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams
]

{ #category : 'tests - protocol - prompts/get' }
McpServerTest >> testPromptsGetMissingName [

	| response error |
	response := self sendRequest: 'prompts/get' id: 35 params: Dictionary new.
	error := response at: 'error'.
	self assert: (error at: 'code') equals: McpConstants errorCodeInvalidParams
]

{ #category : 'tests - error handling' }
McpServerTest >> testInvalidJsonMessage [

	transport simulateReceive: 'this is not valid json{{{'.
	"Server should send a parse error response"
	self assert: transport sentMessages isNotEmpty.
	self
		assert: ((transport lastSentParsed at: 'error') at: 'code')
		equals: McpConstants errorCodeParseError
]

{ #category : 'tests - json-rpc compliance' }
McpServerTest >> testResponseHasJsonrpc [

	| response |
	response := self sendRequest: 'ping' id: 50.
	self assert: (response at: 'jsonrpc') equals: '2.0'
]

{ #category : 'tests - json-rpc compliance' }
McpServerTest >> testResponseHasMatchingId [

	| response |
	response := self sendRequest: 'ping' id: 777.
	self assert: (response at: 'id') equals: 777
]

{ #category : 'tests - json-rpc compliance' }
McpServerTest >> testResponseHasStringId [

	| response |
	response := self sendRequest: 'ping' id: 'abc-123'.
	self assert: (response at: 'id') equals: 'abc-123'
]

{ #category : 'tests - integration' }
McpServerTest >> testFullInitializationFlow [

	| initResponse |
	"Step 1: Client sends initialize"
	initResponse := self sendRequest: 'initialize' id: 1.
	self assert: (initResponse includesKey: 'result').

	"Step 2: Client sends initialized notification"
	self deny: server isInitialized.
	self sendNotification: 'notifications/initialized'.
	self assert: server isInitialized.

	"Step 3: Client can now make requests"
	server addTool: (McpTool
			 name: 'hello'
			 description: 'Hello'
			 handler: [ :a | McpToolResult successText: 'world' ]).
	self
		assert: (((self sendRequest: 'tools/list' id: 2) at: 'result') at: 'tools') size
		equals: 1
]
