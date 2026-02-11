Class {
	#name : 'McpToolTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpToolTest >> testAsDictionary [

	| tool schema dict |
	schema := McpSchema
		          object: { 'code' -> (McpSchema string: 'Pharo code to evaluate') }
		          required: #( 'code' ).
	tool := McpTool
		        name: 'evaluate'
		        description: 'Evaluates Pharo code'
		        inputSchema: schema
		        handler: [ :args | McpToolResult successText: 'done' ].
	dict := tool asDictionary.
	self assert: (dict at: 'name') equals: 'evaluate'.
	self assert: (dict at: 'description') equals: 'Evaluates Pharo code'.
	self assert: ((dict at: 'inputSchema') at: 'type') equals: 'object'.
	self
		assert:
		((dict at: 'inputSchema') at: 'required')
		equals: #( 'code' )
]

{ #category : 'tests' }
McpToolTest >> testAsDictionaryWithoutDescription [

	| tool dict |
	tool := McpTool new
		        name: 'simple';
		        inputSchema: (McpSchema object: {  });
		        handler: [ :args | McpToolResult successText: 'ok' ];
		        yourself.
	dict := tool asDictionary.
	self assert: (dict at: 'name') equals: 'simple'.
	self deny: (dict includesKey: 'description')
]

{ #category : 'tests' }
McpToolTest >> testCreateWithNameDescriptionAndHandler [

	| tool |
	tool := McpTool
		        name: 'echo'
		        description: 'Echoes input'
		        handler: [ :args | McpToolResult successText: (args at: 'message') ].
	self assert: tool name equals: 'echo'.
	self assert: tool description equals: 'Echoes input'.
	self assert: tool inputSchema isNotNil.
	self assert: tool handler isNotNil
]

{ #category : 'tests' }
McpToolTest >> testCreateWithSchema [

	| tool schema |
	schema := McpSchema
		          object: { 'message' -> (McpSchema string: 'The message to echo') }
		          required: #( 'message' ).
	tool := McpTool
		        name: 'echo'
		        description: 'Echoes input'
		        inputSchema: schema
		        handler: [ :args | McpToolResult successText: (args at: 'message') ].
	self assert: tool name equals: 'echo'.
	self assert: tool inputSchema equals: schema
]

{ #category : 'tests' }
McpToolTest >> testExecuteHandlesErrors [

	| tool result |
	tool := McpTool
		        name: 'fail'
		        description: 'Always fails'
		        handler: [ :args | self error: 'Intentional failure' ].
	result := tool executeWith: Dictionary new.
	self assert: result isError.
	self assert: (result content first text includesSubstring: 'Intentional failure')
]

{ #category : 'tests' }
McpToolTest >> testExecuteReturnsResult [

	| tool result |
	tool := McpTool
		        name: 'greet'
		        description: 'Greets a user'
		        handler: [ :args |
			        McpToolResult successText: 'Hello, ' , (args at: 'name') ].
	result := tool executeWith: (Dictionary new at: 'name' put: 'World'; yourself).
	self deny: result isError.
	self assert: result content first text equals: 'Hello, World'
]
