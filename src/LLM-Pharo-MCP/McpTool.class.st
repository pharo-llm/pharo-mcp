Class {
	#name : 'McpTool',
	#superclass : 'Object',
	#instVars : [
		'name',
		'description',
		'inputSchema',
		'handler'
	],
	#category : 'LLM-Pharo-MCP-Tools',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Tools'
}

{ #category : 'instance creation' }
McpTool class >> name: aName description: aDescription handler: aBlock [
	"Create a tool with no required input parameters"

	^ self
		  name: aName
		  description: aDescription
		  inputSchema: (McpSchema object: {  })
		  handler: aBlock
]

{ #category : 'instance creation' }
McpTool class >> name: aName description: aDescription inputSchema: aSchema handler: aBlock [

	^ self new
		  name: aName;
		  description: aDescription;
		  inputSchema: aSchema;
		  handler: aBlock;
		  yourself
]

{ #category : 'converting' }
McpTool >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'name' put: name.
	description ifNotNil: [ dict at: 'description' put: description ].
	dict at: 'inputSchema' put: inputSchema asDictionary.
	^ dict
]

{ #category : 'accessing' }
McpTool >> description [

	^ description
]

{ #category : 'accessing' }
McpTool >> description: anObject [

	description := anObject
]

{ #category : 'executing' }
McpTool >> executeWith: arguments [
	"Execute this tool with the given arguments dictionary.
	 Returns an McpToolResult."

	[ ^ handler value: arguments ]
		on: Error
		do: [ :ex | ^ McpToolResult errorText: ex messageText ]
]

{ #category : 'accessing' }
McpTool >> handler [

	^ handler
]

{ #category : 'accessing' }
McpTool >> handler: anObject [

	handler := anObject
]

{ #category : 'accessing' }
McpTool >> inputSchema [

	^ inputSchema
]

{ #category : 'accessing' }
McpTool >> inputSchema: anObject [

	inputSchema := anObject
]

{ #category : 'accessing' }
McpTool >> name [

	^ name
]

{ #category : 'accessing' }
McpTool >> name: anObject [

	name := anObject
]
