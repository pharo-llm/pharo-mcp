Class {
	#name : 'McpContentItemTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests - creation' }
McpContentItemTest >> testTextContent [

	| item |
	item := McpContentItem text: 'Hello world'.
	self assert: item type equals: 'text'.
	self assert: item text equals: 'Hello world'.
	self assert: item isText.
	self deny: item isImage.
	self deny: item isResource
]

{ #category : 'tests - creation' }
McpContentItemTest >> testImageContent [

	| item |
	item := McpContentItem image: 'base64data==' mimeType: 'image/png'.
	self assert: item type equals: 'image'.
	self assert: item data equals: 'base64data=='.
	self assert: item mimeType equals: 'image/png'.
	self assert: item isImage.
	self deny: item isText
]

{ #category : 'tests - creation' }
McpContentItemTest >> testResourceContent [

	| item |
	item := McpContentItem resource: 'file://test.txt' text: 'content' mimeType: 'text/plain'.
	self assert: item type equals: 'resource'.
	self assert: item uri equals: 'file://test.txt'.
	self assert: item text equals: 'content'.
	self assert: item mimeType equals: 'text/plain'.
	self assert: item isResource
]

{ #category : 'tests - converting' }
McpContentItemTest >> testTextContentAsDictionary [

	| item dict |
	item := McpContentItem text: 'Hello'.
	dict := item asDictionary.
	self assert: (dict at: 'type') equals: 'text'.
	self assert: (dict at: 'text') equals: 'Hello'.
	self deny: (dict includesKey: 'data').
	self deny: (dict includesKey: 'resource')
]

{ #category : 'tests - converting' }
McpContentItemTest >> testImageContentAsDictionary [

	| item dict |
	item := McpContentItem image: 'abc123' mimeType: 'image/jpeg'.
	dict := item asDictionary.
	self assert: (dict at: 'type') equals: 'image'.
	self assert: (dict at: 'data') equals: 'abc123'.
	self assert: (dict at: 'mimeType') equals: 'image/jpeg'
]

{ #category : 'tests - converting' }
McpContentItemTest >> testResourceContentAsDictionary [

	| item dict resource |
	item := McpContentItem
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
