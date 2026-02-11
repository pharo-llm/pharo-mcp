Class {
	#name : 'McpResource',
	#superclass : 'Object',
	#instVars : [
		'uri',
		'name',
		'description',
		'mimeType',
		'handler'
	],
	#category : 'LLM-Pharo-MCP-Ressources',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Ressources'
}

{ #category : 'as yet unclassified' }
McpResource class >> uri: aUri name: aName description: aDescription handler: aBlock [

	^ self
		  uri: aUri
		  name: aName
		  description: aDescription
		  mimeType: 'text/plain'
		  handler: aBlock
]

{ #category : 'instance creation' }
McpResource class >> uri: aUri name: aName description: aDescription mimeType: aMimeType handler: aBlock [

	^ self new
		  uri: aUri;
		  name: aName;
		  description: aDescription;
		  mimeType: aMimeType;
		  handler: aBlock;
		  yourself
]

{ #category : 'converting' }
McpResource >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'uri' put: uri.
	dict at: 'name' put: name.
	description ifNotNil: [ dict at: 'description' put: description ].
	mimeType ifNotNil: [ dict at: 'mimeType' put: mimeType ].
	^ dict
]

{ #category : 'accessing' }
McpResource >> description [

	^ description
]

{ #category : 'accessing' }
McpResource >> description: anObject [

	description := anObject
]

{ #category : 'accessing' }
McpResource >> handler [

	^ handler
]

{ #category : 'accessing' }
McpResource >> handler: anObject [

	handler := anObject
]

{ #category : 'accessing' }
McpResource >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
McpResource >> mimeType: anObject [

	mimeType := anObject
]

{ #category : 'accessing' }
McpResource >> name [

	^ name
]

{ #category : 'accessing' }
McpResource >> name: anObject [

	name := anObject
]

{ #category : 'executing' }
McpResource >> read [
	"Read this resource, returns an array of content items"

	[ ^ handler value ]
		on: Error
		do: [ :ex |
			^ { MCPContentItem text: 'Error reading resource: ' , ex messageText } ]
]

{ #category : 'accessing' }
McpResource >> uri [

	^ uri
]

{ #category : 'accessing' }
McpResource >> uri: anObject [

	uri := anObject
]
