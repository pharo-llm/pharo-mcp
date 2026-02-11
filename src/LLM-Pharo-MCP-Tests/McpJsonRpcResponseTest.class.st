Class {
	#name : 'McpJsonRpcResponseTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpJsonRpcResponseTest >> testAsJsonString [

	| resp json parsed |
	resp := McpJsonRpcResponse id: 1 result: (Dictionary new at: 'ok' put: true; yourself).
	json := resp asJsonString.
	parsed := STONJSON fromString: json.
	self assert: (parsed at: 'jsonrpc') equals: '2.0'.
	self assert: (parsed at: 'id') equals: 1.
	self assert: ((parsed at: 'result') at: 'ok') equals: true
]

{ #category : 'tests' }
McpJsonRpcResponseTest >> testCreateErrorResponse [

	| resp error |
	error := McpJsonRpcError code: -32601 message: 'Method not found'.
	resp := McpJsonRpcResponse id: 2 error: error.
	self assert: resp id equals: 2.
	self assert: resp result isNil.
	self assert: resp error equals: error.
	self deny: resp isSuccess.
	self assert: resp isError
]

{ #category : 'tests' }
McpJsonRpcResponseTest >> testCreateSuccessResponse [

	| resp result |
	result := Dictionary new at: 'status' put: 'ok'; yourself.
	resp := McpJsonRpcResponse id: 1 result: result.
	self assert: resp id equals: 1.
	self assert: (resp result at: 'status') equals: 'ok'.
	self assert: resp error isNil.
	self assert: resp isSuccess.
	self deny: resp isError
]

{ #category : 'tests' }
McpJsonRpcResponseTest >> testErrorAsDictionary [

	| resp dict error |
	error := McpJsonRpcError code: -32600 message: 'Invalid'.
	resp := McpJsonRpcResponse id: 3 error: error.
	dict := resp asDictionary.
	self assert: (dict at: 'jsonrpc') equals: '2.0'.
	self assert: (dict at: 'id') equals: 3.
	self assert: ((dict at: 'error') at: 'code') equals: -32600.
	self assert: ((dict at: 'error') at: 'message') equals: 'Invalid'.
	self deny: (dict includesKey: 'result')
]

{ #category : 'tests' }
McpJsonRpcResponseTest >> testSuccessAsDictionary [

	| resp dict result |
	result := Dictionary new at: 'data' put: 'hello'; yourself.
	resp := McpJsonRpcResponse id: 1 result: result.
	dict := resp asDictionary.
	self assert: (dict at: 'jsonrpc') equals: '2.0'.
	self assert: (dict at: 'id') equals: 1.
	self assert: ((dict at: 'result') at: 'data') equals: 'hello'.
	self deny: (dict includesKey: 'error')
]

{ #category : 'tests' }
McpJsonRpcResponseTest >> testSuccessWithNilResult [

	| resp dict |
	resp := McpJsonRpcResponse id: 1 result: nil.
	dict := resp asDictionary.
	self assert: (dict at: 'result') equals: Dictionary new
]
