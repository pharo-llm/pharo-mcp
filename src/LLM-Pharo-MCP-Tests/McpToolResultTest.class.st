Class {
	#name : 'McpToolResultTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests - creation' }
McpToolResultTest >> testSuccessText [

	| result |
	result := McpToolResult successText: 'All good'.
	self deny: result isError.
	self assert: result content size equals: 1.
	self assert: result content first text equals: 'All good'.
	self assert: result content first isText
]

{ #category : 'tests - creation' }
McpToolResultTest >> testErrorText [

	| result |
	result := McpToolResult errorText: 'Something failed'.
	self assert: result isError.
	self assert: result content size equals: 1.
	self assert: result content first text equals: 'Something failed'
]

{ #category : 'tests - creation' }
McpToolResultTest >> testSuccessWithMultipleContent [

	| result items |
	items := {
		         McpContentItem text: 'Line 1'.
		         McpContentItem text: 'Line 2' }.
	result := McpToolResult success: items.
	self deny: result isError.
	self assert: result content size equals: 2
]

{ #category : 'tests - creation' }
McpToolResultTest >> testDefaultInitialization [

	| result |
	result := McpToolResult new.
	self deny: result isError.
	self assert: result content isEmpty
]

{ #category : 'tests - converting' }
McpToolResultTest >> testSuccessAsDictionary [

	| result dict |
	result := McpToolResult successText: 'OK'.
	dict := result asDictionary.
	self assert: (dict at: 'content') size equals: 1.
	self assert: ((dict at: 'content') first at: 'type') equals: 'text'.
	self assert: ((dict at: 'content') first at: 'text') equals: 'OK'.
	self deny: (dict includesKey: 'isError')
]

{ #category : 'tests - converting' }
McpToolResultTest >> testErrorAsDictionary [

	| result dict |
	result := McpToolResult errorText: 'Failed'.
	dict := result asDictionary.
	self assert: (dict at: 'isError') equals: true.
	self assert: ((dict at: 'content') first at: 'text') equals: 'Failed'
]
