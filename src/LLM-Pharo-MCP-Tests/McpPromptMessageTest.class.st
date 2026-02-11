Class {
	#name : 'McpPromptMessageTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpPromptMessageTest >> testAsDictionary [

	| msg dict |
	msg := McpPromptMessage user: 'Test message'.
	dict := msg asDictionary.
	self assert: (dict at: 'role') equals: 'user'.
	self assert: ((dict at: 'content') at: 'type') equals: 'text'.
	self assert: ((dict at: 'content') at: 'text') equals: 'Test message'
]

{ #category : 'tests' }
McpPromptMessageTest >> testAssistantMessage [

	| msg |
	msg := McpPromptMessage assistant: 'Hi there'.
	self assert: msg role equals: 'assistant'.
	self assert: msg content text equals: 'Hi there'
]

{ #category : 'tests' }
McpPromptMessageTest >> testCustomRoleAndContent [

	| msg content |
	content := MCPContentItem image: 'base64' mimeType: 'image/png'.
	msg := McpPromptMessage role: 'user' content: content.
	self assert: msg role equals: 'user'.
	self assert: msg content isImage
]

{ #category : 'tests' }
McpPromptMessageTest >> testUserMessage [

	| msg |
	msg := McpPromptMessage user: 'Hello'.
	self assert: msg role equals: 'user'.
	self assert: msg content isText.
	self assert: msg content text equals: 'Hello'
]
