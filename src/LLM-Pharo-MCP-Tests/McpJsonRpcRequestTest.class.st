Class {
	#name : 'McpJsonRpcRequestTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpJsonRpcRequestTest >> testAsDictionary [

	| req dict |
	req := McpJsonRpcRequest id: 1 method: 'ping'.
	dict := req asDictionary.
	self assert: (dict at: 'jsonrpc') equals: '2.0'.
	self assert: (dict at: 'id') equals: 1.
	self assert: (dict at: 'method') equals: 'ping'.
	self deny: (dict includesKey: 'params')
]

{ #category : 'tests' }
McpJsonRpcRequestTest >> testAsDictionaryWithParams [

	| req dict params |
	params := Dictionary new at: 'uri' put: 'file://test'; yourself.
	req := McpJsonRpcRequest id: 5 method: 'resources/read' params: params.
	dict := req asDictionary.
	self assert: (dict at: 'jsonrpc') equals: '2.0'.
	self assert: (dict at: 'id') equals: 5.
	self assert: (dict at: 'method') equals: 'resources/read'.
	self assert: ((dict at: 'params') at: 'uri') equals: 'file://test'
]

{ #category : 'tests' }
McpJsonRpcRequestTest >> testAsJsonString [

	| req json parsed |
	req := McpJsonRpcRequest id: 1 method: 'ping'.
	json := req asJsonString.
	parsed := STONJSON fromString: json.
	self assert: (parsed at: 'jsonrpc') equals: '2.0'.
	self assert: (parsed at: 'id') equals: 1.
	self assert: (parsed at: 'method') equals: 'ping'
]

{ #category : 'tests' }
McpJsonRpcRequestTest >> testCreateWithIdAndMethod [

	| req |
	req := McpJsonRpcRequest id: 1 method: 'initialize'.
	self assert: req id equals: 1.
	self assert: req method equals: 'initialize'.
	self assert: req params isNil.
	self assert: req jsonrpc equals: '2.0'
]

{ #category : 'tests' }
McpJsonRpcRequestTest >> testCreateWithIdMethodAndParams [

	| req params |
	params := Dictionary new at: 'name' put: 'test'; yourself.
	req := McpJsonRpcRequest id: 42 method: 'tools/call' params: params.
	self assert: req id equals: 42.
	self assert: req method equals: 'tools/call'.
	self assert: (req params at: 'name') equals: 'test'
]

{ #category : 'tests' }
McpJsonRpcRequestTest >> testIsRequest [

	| req |
	req := McpJsonRpcRequest id: 1 method: 'ping'.
	self assert: req isRequest
]
