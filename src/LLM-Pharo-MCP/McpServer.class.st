Class {
	#name : 'McpServer',
	#superclass : 'Object',
	#instVars : [
		'name',
		'version',
		'capabilities',
		'tools',
		'resources',
		'resourceTemplates',
		'prompts',
		'transport',
		'initialized'
	],
	#category : 'LLM-Pharo-MCP-Server',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Server'
}

{ #category : 'instance creation' }
McpServer class >> default [

	^ self
		  name: MCPConstants mcpServerName
		  version: MCPConstants mcpServerVersion
]

{ #category : 'instance creation' }
McpServer class >> name: aName version: aVersion [

	^ self new
		  name: aName;
		  version: aVersion;
		  yourself
]

{ #category : 'prompt' }
McpServer >> addPrompt: aMcpPrompt [

	prompts at: aMcpPrompt name put: aMcpPrompt
]

{ #category : 'ressources' }
McpServer >> addResource: aMcpResource [

	resources at: aMcpResource uri put: aMcpResource
]

{ #category : 'ressources' }
McpServer >> addResourceTemplate: aMcpResourceTemplate [

	resourceTemplates
		at: aMcpResourceTemplate uriTemplate
		put: aMcpResourceTemplate
]

{ #category : 'tools' }
McpServer >> addTool: aMcpTool [

	tools at: aMcpTool name put: aMcpTool
]

{ #category : 'initialization' }
McpServer >> capabilities [

	^ capabilities
]

{ #category : 'initialization' }
McpServer >> capabilities: anObject [

	capabilities := anObject
]

{ #category : 'dispatching' }
McpServer >> dispatchRequest: aRequest [
	"Dispatch a request to the correct method handler"

	| method |
	method := aRequest method.

	method = 'initialize' ifTrue: [
		^ self handleInitialize: aRequest ].
	method = 'ping' ifTrue: [ ^ self handlePing: aRequest ].
	method = 'tools/list' ifTrue: [
		^ self handleToolsList: aRequest ].
	method = 'tools/call' ifTrue: [
		^ self handleToolsCall: aRequest ].
	method = 'resources/list' ifTrue: [
		^ self handleResourcesList: aRequest ].
	method = 'resources/read' ifTrue: [
		^ self handleResourcesRead: aRequest ].
	method = 'resources/templates/list' ifTrue: [
		^ self handleResourcesTemplatesList: aRequest ].
	method = 'prompts/list' ifTrue: [
		^ self handlePromptsList: aRequest ].
	method = 'prompts/get' ifTrue: [
		^ self handlePromptsGet: aRequest ].

	^ McpJsonRpcResponse
		  id: aRequest id
		  error: (McpJsonRpcError methodNotFound: method)
]

{ #category : 'handler' }
McpServer >> handleIncomingMessage: aJsonString [
	"Handle a raw JSON string message from the transport"

	| message |
	[
	message := MCPJSONRPCMessage fromJsonString: aJsonString.
	(message isKindOf: McpJsonRpcRequest) ifTrue: [
		self handleRequest: message ].
	(message isKindOf: McpJsonRpcNotification) ifTrue: [
		self handleNotification: message ] ]
		on: Error
		do: [ :ex |
			| errorResponse |
			errorResponse := McpJsonRpcResponse
				                  id: nil
				                  error: (McpJsonRpcError
						                   code: MCPConstants errorCodeParseError
						                   message: ex messageText).
			self sendResponse: errorResponse ]
]

{ #category : 'handler' }
McpServer >> handleInitialize: aRequest [

	| result |
	result := OrderedDictionary new.
	result at: 'protocolVersion' put: MCPConstants mcpProtocolVersion.
	result at: 'capabilities' put: capabilities asDictionary.
	result at: 'serverInfo' put: (OrderedDictionary new
			 at: 'name' put: name;
			 at: 'version' put: version;
			 yourself).
	^ McpJsonRpcResponse id: aRequest id result: result
]

{ #category : 'handler' }
McpServer >> handleNotification: aNotification [ 
	"Handle a JSON-RPC notification (no response expected)"

	aNotification method = 'notifications/initialized' ifTrue: [
		initialized := true ].
	"Other notifications can be handled here in the future"
]

{ #category : 'handler' }
McpServer >> handlePing: aRequest [

	^ McpJsonRpcResponse
		  id: aRequest id
		  result: (OrderedDictionary new)
]

{ #category : 'handler' }
McpServer >> handlePromptsGet: aRequest [

	| params promptName args prompt result |
	params := aRequest params.
	params ifNil: [
		^ McpJsonRpcResponse
			  id: aRequest id
			  error: (McpJsonRpcError invalidParams: 'Missing params') ].
	promptName := params at: 'name' ifAbsent: [
		              ^ McpJsonRpcResponse
			                id: aRequest id
			                error:
			                (McpJsonRpcError invalidParams:
				                 'Missing prompt name') ].
	args := params at: 'arguments' ifAbsent: [ Dictionary new ].
	prompt := self promptNamed: promptName.
	prompt ifNil: [
		^ McpJsonRpcResponse
			  id: aRequest id
			  error: (McpJsonRpcError invalidParams:
					   'Unknown prompt: ' , promptName) ].
	result := prompt getWithArguments: args.
	^ McpJsonRpcResponse id: aRequest id result: result
]

{ #category : 'handler' }
McpServer >> handlePromptsList: aRequest [

	| result promptList |
	promptList := prompts values collect: [ :each | each asDictionary ].
	result := OrderedDictionary new.
	result at: 'prompts' put: promptList asArray.
	^ McpJsonRpcResponse id: aRequest id result: result
]

{ #category : 'handler' }
McpServer >> handleRequest: aRequest [
	"Dispatch a JSON-RPC request to the appropriate handler"

	| response |
	response := self dispatchRequest: aRequest.
	self sendResponse: response
]

{ #category : 'handler' }
McpServer >> handleResourcesList: aRequest [

	| result resourceList |
	resourceList := resources values collect: [ :each | each asDictionary ].
	result := OrderedDictionary new.
	result at: 'resources' put: resourceList asArray.
	^ McpJsonRpcResponse id: aRequest id result: result
]

{ #category : 'handler' }
McpServer >> handleResourcesRead: aRequest [

	| params uri resource contents template |
	params := aRequest params.
	params ifNil: [
		^ McpJsonRpcResponse
			  id: aRequest id
			  error: (McpJsonRpcError invalidParams: 'Missing params') ].
	uri := params at: 'uri' ifAbsent: [
		       ^ McpJsonRpcResponse
			         id: aRequest id
			         error:
			         (McpJsonRpcError invalidParams: 'Missing resource URI') ].

	"Try exact resource match first"
	resource := self resourceAtUri: uri.
	resource ifNotNil: [
		contents := resource read.
		^ McpJsonRpcResponse id: aRequest id result: (OrderedDictionary new
				   at: 'contents'
				   put: (contents collect: [ :c | c asDictionary ]);
				   yourself) ].

	"Try resource templates"
	template := resourceTemplates values
		            detect: [ :t | (t extractParams: uri) isNotNil ]
		            ifNone: [ nil ].
	template ifNotNil: [
		contents := template readWithUri: uri.
		^ McpJsonRpcResponse id: aRequest id result: (OrderedDictionary new
				   at: 'contents'
				   put: (contents collect: [ :c | c asDictionary ]);
				   yourself) ].

	^ McpJsonRpcResponse
		  id: aRequest id
		  error: (McpJsonRpcError invalidParams: 'Resource not found: ' , uri)
]

{ #category : 'handler' }
McpServer >> handleResourcesTemplatesList: aRequest [

	| result templateList |
	templateList := resourceTemplates values collect: [ :each |
		                each asDictionary ].
	result := OrderedDictionary new.
	result at: 'resourceTemplates' put: templateList asArray.
	^ McpJsonRpcResponse id: aRequest id result: result
]

{ #category : 'handler' }
McpServer >> handleToolsCall: aRequest [

	| params toolName arguments tool toolResult |
	params := aRequest params.
	params ifNil: [
		^ McpJsonRpcResponse
			  id: aRequest id
			  error: (McpJsonRpcError invalidParams: 'Missing params') ].
	toolName := params at: 'name' ifAbsent: [
		            ^ McpJsonRpcResponse
			              id: aRequest id
			              error:
			              (McpJsonRpcError invalidParams:
				               'Missing tool name') ].
	arguments := params at: 'arguments' ifAbsent: [ Dictionary new ].
	tool := self toolNamed: toolName.
	tool ifNil: [
		^ McpJsonRpcResponse
			  id: aRequest id
			  error:
			  (McpJsonRpcError invalidParams: 'Unknown tool: ' , toolName) ].
	toolResult := tool executeWith: arguments.
	^ McpJsonRpcResponse id: aRequest id result: toolResult asDictionary
]

{ #category : 'handler' }
McpServer >> handleToolsList: aRequest [

	| result toolList |
	toolList := tools values collect: [ :each | each asDictionary ].
	result := OrderedDictionary new.
	result at: 'tools' put: toolList asArray.
	^ McpJsonRpcResponse id: aRequest id result: result
]

{ #category : 'initialization' }
McpServer >> initialize [

	super initialize.
	name := MCPConstants mcpServerName.
	version := MCPConstants mcpServerVersion.
	capabilities := McpServerCapabilities new.
	tools := OrderedDictionary new.
	resources := OrderedDictionary new.
	resourceTemplates := OrderedDictionary new.
	prompts := OrderedDictionary new.
	initialized := false
]

{ #category : 'initialization' }
McpServer >> isInitialized [

	^ initialized
]

{ #category : 'initialization' }
McpServer >> name [

	^ name
]

{ #category : 'initialization' }
McpServer >> name: anObject [

	name := anObject
]

{ #category : 'prompt' }
McpServer >> promptNamed: aName [

	^ prompts at: aName ifAbsent: [ nil ]
]

{ #category : 'ressources' }
McpServer >> prompts [

	^ prompts
]

{ #category : 'prompt' }
McpServer >> removePrompt: aName [

	prompts removeKey: aName ifAbsent: [  ]
]

{ #category : 'ressources' }
McpServer >> removeResource: aUri [

	resources removeKey: aUri ifAbsent: [  ]
]

{ #category : 'tools' }
McpServer >> removeTool: aName [

	tools removeKey: aName ifAbsent: [  ]
]

{ #category : 'ressources' }
McpServer >> resourceAtUri: aUri [

	^ resources at: aUri ifAbsent: [ nil ]
]

{ #category : 'ressources' }
McpServer >> resourceTemplates [

	^ resourceTemplates
]

{ #category : 'ressources' }
McpServer >> resources [

	^ resources
]

{ #category : 'handler' }
McpServer >> sendResponse: aResponse [

	transport ifNotNil: [
		transport sendMessage: aResponse asJsonString ]
]

{ #category : 'running' }
McpServer >> start [
	"Start the server with the configured transport"

	transport ifNil: [ self error: 'Transport must be set before starting' ].
	transport messageHandler: [ :msg | self handleIncomingMessage: msg ].
	transport start
]

{ #category : 'running' }
McpServer >> stop [

	transport ifNotNil: [ transport stop ]
]

{ #category : 'tools' }
McpServer >> toolNamed: aName [

	^ tools at: aName ifAbsent: [ nil ]
]

{ #category : 'tools' }
McpServer >> tools [

	^ tools
]

{ #category : 'initialization' }
McpServer >> transport [

	^ transport
]

{ #category : 'initialization' }
McpServer >> transport: anObject [

	transport := anObject
]

{ #category : 'initialization' }
McpServer >> version [

	^ version
]

{ #category : 'initialization' }
McpServer >> version: anObject [

	version := anObject
]
