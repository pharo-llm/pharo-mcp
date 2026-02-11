Class {
	#name : 'McpContentItemTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests' }
McpContentItemTest >> testImageContent [

	| item |
	item := MCPContentItem image: 'base64data==' mimeType: 'image/png'.
	self assert: item type equals: 'image'.
	self assert: item data equals: 'base64data=='.
	self assert: item mimeType equals: 'image/png'.
	self assert: item isImage.
	self deny: item isText
]

{ #category : 'tests' }
McpContentItemTest >> testImageContentAsDictionary [

	| item dict |
	item := MCPContentItem image: 'abc123' mimeType: 'image/jpeg'.
	dict := item asDictionary.
	self assert: (dict at: 'type') equals: 'image'.
	self assert: (dict at: 'data') equals: 'abc123'.
	self assert: (dict at: 'mimeType') equals: 'image/jpeg'
]

{ #category : 'tests' }
McpContentItemTest >> testResourceContent [

	| item |
	item := MCPContentItem resource: 'file://test.txt' text: 'content' mimeType: 'text/plain'.
	self assert: item type equals: 'resource'.
	self assert: item uri equals: 'file://test.txt'.
	self assert: item text equals: 'content'.
	self assert: item mimeType equals: 'text/plain'.
	self assert: item isResource
]

{ #category : 'tests' }
McpContentItemTest >> testResourceContentAsDictionary [

	| item dict resource |
	item := MCPContentItem
		        resource: 'pharo://class/Object'
		        text: 'Object class source'
		        mimeType: 'text/plain'.
	dict := item asDictionary.
	self assert: (dict at: 'type') equals: 'resource'.
	resource := dict at: 'resource'.
	self assert: (resource at: 'uri') equals: 'pharo://class/Object'.
	self assert: (resource at: 'text') equals: 'Object class source'.
	self assert: (resource at: 'mimeType') equals: 'text/plain'
]

{ #category : 'tests' }
McpContentItemTest >> testTextContent [

	| item |
	item := MCPContentItem text: 'Hello world'.
	self assert: item type equals: 'text'.
	self assert: item text equals: 'Hello world'.
	self assert: item isText.
	self deny: item isImage.
	self deny: item isResource
]

{ #category : 'tests' }
McpContentItemTest >> testTextContentAsDictionary [

	| item dict |
	item := MCPContentItem text: 'Hello'.
	dict := item asDictionary.
	self assert: (dict at: 'type') equals: 'text'.
	self assert: (dict at: 'text') equals: 'Hello'.
	self deny: (dict includesKey: 'data').
	self deny: (dict includesKey: 'resource')
]
