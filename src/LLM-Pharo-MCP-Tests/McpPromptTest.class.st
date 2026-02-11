Class {
	#name : 'McpPromptTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests - creation' }
McpPromptTest >> testCreateWithoutArguments [

	| prompt |
	prompt := McpPrompt
		          name: 'review-code'
		          description: 'Review Pharo code'
		          handler: [ :args |
			          Dictionary new
				          at: 'description' put: 'Code review';
				          at: 'messages'
				          put: { (McpPromptMessage user: 'Review this code') asDictionary };
				          yourself ].
	self assert: prompt name equals: 'review-code'.
	self assert: prompt description equals: 'Review Pharo code'.
	self assert: prompt arguments isEmpty
]

{ #category : 'tests - creation' }
McpPromptTest >> testCreateWithArguments [

	| prompt args |
	args := {
		        McpPromptArgument required: 'code' description: 'The code to review'.
		        McpPromptArgument name: 'style' description: 'Review style' }.
	prompt := McpPrompt
		          name: 'review-code'
		          description: 'Review code'
		          arguments: args
		          handler: [ :a | Dictionary new ].
	self assert: prompt arguments size equals: 2.
	self assert: prompt arguments first isRequired.
	self deny: prompt arguments second isRequired
]

{ #category : 'tests - execution' }
McpPromptTest >> testGetWithArguments [

	| prompt result |
	prompt := McpPrompt
		          name: 'greet'
		          description: 'Greet a user'
		          arguments: { McpPromptArgument required: 'name' description: 'User name' }
		          handler: [ :args |
			          Dictionary new
				          at: 'description' put: 'Greeting for ' , (args at: 'name');
				          at: 'messages'
				          put:
					          { (McpPromptMessage user: 'Hello ' , (args at: 'name'))
						          asDictionary };
				          yourself ].
	result := prompt getWithArguments: (Dictionary new at: 'name' put: 'Alice'; yourself).
	self assert: (result at: 'description') equals: 'Greeting for Alice'.
	self assert: (result at: 'messages') size equals: 1
]

{ #category : 'tests - execution' }
McpPromptTest >> testGetWithMissingRequiredArgument [

	| prompt |
	prompt := McpPrompt
		          name: 'greet'
		          description: 'Greet'
		          arguments: { McpPromptArgument required: 'name' description: 'Name' }
		          handler: [ :args | Dictionary new ].
	self
		should: [ prompt getWithArguments: Dictionary new ]
		raise: Error
]

{ #category : 'tests - execution' }
McpPromptTest >> testGetWithOptionalArgumentMissing [

	| prompt result |
	prompt := McpPrompt
		          name: 'analyze'
		          description: 'Analyze'
		          arguments: { McpPromptArgument name: 'depth' description: 'Analysis depth' }
		          handler: [ :args |
			          Dictionary new
				          at: 'description' put: 'Analysis';
				          at: 'messages' put: #();
				          yourself ].
	result := prompt getWithArguments: Dictionary new.
	self assert: (result at: 'description') equals: 'Analysis'
]

{ #category : 'tests - converting' }
McpPromptTest >> testAsDictionary [

	| prompt dict |
	prompt := McpPrompt
		          name: 'review'
		          description: 'Code review'
		          arguments: { McpPromptArgument required: 'code' description: 'The code' }
		          handler: [ :a | Dictionary new ].
	dict := prompt asDictionary.
	self assert: (dict at: 'name') equals: 'review'.
	self assert: (dict at: 'description') equals: 'Code review'.
	self assert: (dict at: 'arguments') size equals: 1.
	self assert: ((dict at: 'arguments') first at: 'name') equals: 'code'
]

{ #category : 'tests - converting' }
McpPromptTest >> testAsDictionaryNoArguments [

	| prompt dict |
	prompt := McpPrompt
		          name: 'simple'
		          description: 'Simple prompt'
		          handler: [ :a | Dictionary new ].
	dict := prompt asDictionary.
	self assert: (dict at: 'name') equals: 'simple'.
	self deny: (dict includesKey: 'arguments')
]
