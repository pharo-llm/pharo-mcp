"
I represent one content item returned by resources or prompts.

"
Class {
	#name : 'McpContentItem',
	#superclass : 'Object',
	#instVars : [
		'type',
		'text',
		'mimeType',
		'data',
		'uri'
	],
	#category : 'LLM-Pharo-MCP-Core',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Core'
}

{ #category : 'accessing' }
McpContentItem class >> image: base64Data mimeType: aMimeType [

	^ self new
		  type: 'image';
		  data: base64Data;
		  mimeType: aMimeType;
		  yourself
]

{ #category : 'accessing' }
McpContentItem class >> resource: aUri text: aString mimeType: aMimeType [

	^ self new
		  type: 'resource';
		  uri: aUri;
		  text: aString;
		  mimeType: aMimeType;
		  yourself
]

{ #category : 'accessing' }
McpContentItem class >> text: aString [

	^ self new
		  type: 'text';
		  text: aString;
		  yourself
]

{ #category : 'accessing' }
McpContentItem >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'type' put: type.
	self isText ifTrue: [ dict at: 'text' put: text ].
	self isImage ifTrue: [
		dict at: 'data' put: data.
		dict at: 'mimeType' put: mimeType ].
	self isResource ifTrue: [
		| resource |
		resource := OrderedDictionary new.
		resource at: 'uri' put: uri.
		text ifNotNil: [ resource at: 'text' put: text ].
		mimeType ifNotNil: [ resource at: 'mimeType' put: mimeType ].
		dict at: 'resource' put: resource ].
	^ dict
]

{ #category : 'accessing' }
McpContentItem >> data [

	^ data
]

{ #category : 'accessing' }
McpContentItem >> data: anObject [

	data := anObject
]

{ #category : 'accessing' }
McpContentItem >> isImage [

	^ type = 'image'
]

{ #category : 'accessing' }
McpContentItem >> isResource [

	^ type = 'resource'
]

{ #category : 'accessing' }
McpContentItem >> isText [

	^ type = 'text'
]

{ #category : 'accessing' }
McpContentItem >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
McpContentItem >> mimeType: anObject [

	mimeType := anObject
]

{ #category : 'accessing' }
McpContentItem >> text [

	^ text
]

{ #category : 'accessing' }
McpContentItem >> text: anObject [

	text := anObject
]

{ #category : 'accessing' }
McpContentItem >> type [

	^ type
]

{ #category : 'accessing' }
McpContentItem >> type: anObject [

	type := anObject
]

{ #category : 'accessing' }
McpContentItem >> uri [

	^ uri
]

{ #category : 'accessing' }
McpContentItem >> uri: anObject [

	uri := anObject
]
