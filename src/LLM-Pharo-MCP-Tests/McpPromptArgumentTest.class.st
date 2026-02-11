Class {
	#name : 'McpPromptArgumentTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests - creation' }
McpPromptArgumentTest >> testCreateOptional [

	| arg |
	arg := McpPromptArgument name: 'language' description: 'Programming language'.
	self assert: arg name equals: 'language'.
	self assert: arg description equals: 'Programming language'.
	self deny: arg isRequired
]

{ #category : 'tests - creation' }
McpPromptArgumentTest >> testCreateRequired [

	| arg |
	arg := McpPromptArgument required: 'code' description: 'Source code to review'.
	self assert: arg name equals: 'code'.
	self assert: arg description equals: 'Source code to review'.
	self assert: arg isRequired
]

{ #category : 'tests - creation' }
McpPromptArgumentTest >> testCreateWithExplicitRequired [

	| arg |
	arg := McpPromptArgument name: 'name' description: 'A name' required: true.
	self assert: arg isRequired.

	arg := McpPromptArgument name: 'name' description: 'A name' required: false.
	self deny: arg isRequired
]

{ #category : 'tests - converting' }
McpPromptArgumentTest >> testAsDictionary [

	| arg dict |
	arg := McpPromptArgument required: 'className' description: 'The class name'.
	dict := arg asDictionary.
	self assert: (dict at: 'name') equals: 'className'.
	self assert: (dict at: 'description') equals: 'The class name'.
	self assert: (dict at: 'required') equals: true
]

{ #category : 'tests - converting' }
McpPromptArgumentTest >> testAsDictionaryOptional [

	| arg dict |
	arg := McpPromptArgument name: 'format' description: 'Output format'.
	dict := arg asDictionary.
	self assert: (dict at: 'required') equals: false
]
