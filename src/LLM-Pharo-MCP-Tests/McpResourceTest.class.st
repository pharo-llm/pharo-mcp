Class {
	#name : 'McpResourceTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpResourceTest >> testAsDictionary [

	| resource dict |
	resource := McpResource
		            uri: 'pharo://packages'
		            name: 'Packages'
		            description: 'Loaded packages'
		            mimeType: 'application/json'
		            handler: [ {  } ].
	dict := resource asDictionary.
	self assert: (dict at: 'uri') equals: 'pharo://packages'.
	self assert: (dict at: 'name') equals: 'Packages'.
	self assert: (dict at: 'description') equals: 'Loaded packages'.
	self assert: (dict at: 'mimeType') equals: 'application/json'
]

{ #category : 'tests' }
McpResourceTest >> testAsDictionaryWithNilFields [

	| resource dict |
	resource := McpResource new
		            uri: 'pharo://test';
		            name: 'Test';
		            yourself.
	dict := resource asDictionary.
	self assert: (dict at: 'uri') equals: 'pharo://test'.
	self assert: (dict at: 'name') equals: 'Test'.
	self deny: (dict includesKey: 'description').
	self deny: (dict includesKey: 'mimeType')
]

{ #category : 'tests' }
McpResourceTest >> testCreateWithAllFields [

	| resource |
	resource := McpResource
		            uri: 'pharo://system/info'
		            name: 'System Info'
		            description: 'Pharo system information'
		            mimeType: 'application/json'
		            handler: [ { MCPContentItem text: '{}' } ].
	self assert: resource uri equals: 'pharo://system/info'.
	self assert: resource name equals: 'System Info'.
	self assert: resource description equals: 'Pharo system information'.
	self assert: resource mimeType equals: 'application/json'
]

{ #category : 'tests' }
McpResourceTest >> testCreateWithDefaults [

	| resource |
	resource := McpResource
		            uri: 'pharo://test'
		            name: 'Test'
		            description: 'A test resource'
		            handler: [ { MCPContentItem text: 'test' } ].
	self assert: resource mimeType equals: 'text/plain'
]

{ #category : 'tests' }
McpResourceTest >> testRead [

	| resource contents |
	resource := McpResource
		            uri: 'pharo://version'
		            name: 'Version'
		            description: 'Pharo version'
		            handler: [ { MCPContentItem text: 'Pharo 13' } ].
	contents := resource read.
	self assert: contents size equals: 1.
	self assert: contents first text equals: 'Pharo 13'
]

{ #category : 'tests' }
McpResourceTest >> testReadHandlesError [

	| resource contents |
	resource := McpResource
		            uri: 'pharo://fail'
		            name: 'Fail'
		            description: 'Always fails'
		            handler: [ self error: 'Read failure' ].
	contents := resource read.
	self assert: contents size equals: 1.
	self assert: (contents first text includesSubstring: 'Read failure')
]
