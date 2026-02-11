Class {
	#name : 'McpJsonRpcMessageTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpJsonRpcMessageTest >> testFromJsonString [

	| json msg |
	json := '{"jsonrpc":"2.0","id":1,"method":"ping"}'.
	msg := MCPJSONRPCMessage fromJsonString: json.
	self assert: (msg isKindOf: McpJsonRpcRequest).
	self assert: msg method equals: 'ping'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testFromJsonStringNotification [

	| json msg |
	json := '{"jsonrpc":"2.0","method":"notifications/initialized"}'.
	msg := MCPJSONRPCMessage fromJsonString: json.
	self assert: (msg isKindOf: McpJsonRpcNotification).
	self assert: msg method equals: 'notifications/initialized'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testParseErrorResponse [

	| dict msg |
	dict := Dictionary new
		        at: 'jsonrpc' put: '2.0';
		        at: 'id' put: 3;
		        at: 'error'
		        put: (Dictionary new
				         at: 'code' put: -32601;
				         at: 'message' put: 'Method not found';
				         yourself);
		        yourself.
	msg := MCPJSONRPCMessage fromDictionary: dict.
	self assert: (msg isKindOf: McpJsonRpcResponse).
	self assert: msg id equals: 3.
	self assert: msg isError.
	self assert: msg error code equals: -32601.
	self assert: msg error message equals: 'Method not found'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testParseErrorResponseWithData [

	| dict msg |
	dict := Dictionary new
		        at: 'jsonrpc' put: '2.0';
		        at: 'id' put: 4;
		        at: 'error'
		        put: (Dictionary new
				         at: 'code' put: -32603;
				         at: 'message' put: 'Internal error';
				         at: 'data' put: 'trace info';
				         yourself);
		        yourself.
	msg := MCPJSONRPCMessage fromDictionary: dict.
	self assert: msg error data equals: 'trace info'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testParseNotification [

	| dict msg |
	dict := Dictionary new
		        at: 'jsonrpc' put: '2.0';
		        at: 'method' put: 'notifications/initialized';
		        yourself.
	msg := MCPJSONRPCMessage fromDictionary: dict.
	self assert: (msg isKindOf: McpJsonRpcNotification).
	self assert: msg method equals: 'notifications/initialized'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testParseRequest [

	| dict msg |
	dict := Dictionary new
		        at: 'jsonrpc' put: '2.0';
		        at: 'id' put: 1;
		        at: 'method' put: 'ping';
		        yourself.
	msg := MCPJSONRPCMessage fromDictionary: dict.
	self assert: (msg isKindOf: McpJsonRpcRequest).
	self assert: msg id equals: 1.
	self assert: msg method equals: 'ping'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testParseRequestWithParams [

	| dict msg |
	dict := Dictionary new
		        at: 'jsonrpc' put: '2.0';
		        at: 'id' put: 5;
		        at: 'method' put: 'tools/call';
		        at: 'params' put: (Dictionary new at: 'name' put: 'echo'; yourself);
		        yourself.
	msg := MCPJSONRPCMessage fromDictionary: dict.
	self assert: (msg isKindOf: McpJsonRpcRequest).
	self assert: msg id equals: 5.
	self assert: msg method equals: 'tools/call'.
	self assert: (msg params at: 'name') equals: 'echo'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testParseSuccessResponse [

	| dict msg |
	dict := Dictionary new
		        at: 'jsonrpc' put: '2.0';
		        at: 'id' put: 1;
		        at: 'result' put: (Dictionary new at: 'status' put: 'ok'; yourself);
		        yourself.
	msg := MCPJSONRPCMessage fromDictionary: dict.
	self assert: (msg isKindOf: McpJsonRpcResponse).
	self assert: msg id equals: 1.
	self assert: msg isSuccess.
	self assert: (msg result at: 'status') equals: 'ok'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testRequestRoundTrip [

	| req json parsed |
	req := McpJsonRpcRequest id: 42 method: 'tools/list'.
	json := req asJsonString.
	parsed := MCPJSONRPCMessage fromJsonString: json.
	self assert: (parsed isKindOf: McpJsonRpcRequest).
	self assert: parsed id equals: 42.
	self assert: parsed method equals: 'tools/list'
]

{ #category : 'tests' }
McpJsonRpcMessageTest >> testResponseRoundTrip [

	| resp json parsed |
	resp := McpJsonRpcResponse
		        id: 10
		        result: (Dictionary new at: 'tools' put: #(); yourself).
	json := resp asJsonString.
	parsed := MCPJSONRPCMessage fromJsonString: json.
	self assert: (parsed isKindOf: McpJsonRpcResponse).
	self assert: parsed id equals: 10.
	self assert: parsed isSuccess
]
