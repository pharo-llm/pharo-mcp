Class {
	#name : 'McpToolResult',
	#superclass : 'Object',
	#instVars : [
		'content',
		'isError'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'instance creation' }
McpToolResult class >> success: contentItems [

	^ self new
		  content: contentItems;
		  isError: false;
		  yourself
]

{ #category : 'instance creation' }
McpToolResult class >> successText: aString [

	^ self success: { McpContentItem text: aString }
]

{ #category : 'instance creation' }
McpToolResult class >> error: contentItems [

	^ self new
		  content: contentItems;
		  isError: true;
		  yourself
]

{ #category : 'instance creation' }
McpToolResult class >> errorText: aString [

	^ self error: { McpContentItem text: aString }
]

{ #category : 'initialization' }
McpToolResult >> initialize [

	super initialize.
	content := #(  ).
	isError := false
]

{ #category : 'accessing' }
McpToolResult >> content [

	^ content
]

{ #category : 'accessing' }
McpToolResult >> content: anObject [

	content := anObject
]

{ #category : 'accessing' }
McpToolResult >> isError [

	^ isError
]

{ #category : 'accessing' }
McpToolResult >> isError: anObject [

	isError := anObject
]

{ #category : 'converting' }
McpToolResult >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'content' put: (content collect: [ :each | each asDictionary ]).
	isError ifTrue: [ dict at: 'isError' put: true ].
	^ dict
]
