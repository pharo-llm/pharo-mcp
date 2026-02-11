Class {
	#name : 'MCPContentItem',
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
MCPContentItem class >> image: base64Data mimeType: aMimeType [

	^ self new
		  type: 'image';
		  data: base64Data;
		  mimeType: aMimeType;
		  yourself
]

{ #category : 'accessing' }
MCPContentItem class >> resource: aUri text: aString mimeType: aMimeType [

	^ self new
		  type: 'resource';
		  uri: aUri;
		  text: aString;
		  mimeType: aMimeType;
		  yourself
]

{ #category : 'accessing' }
MCPContentItem class >> text: aString [

	^ self new
		  type: 'text';
		  text: aString;
		  yourself
]

{ #category : 'accessing' }
MCPContentItem >> asDictionary [

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
MCPContentItem >> data [

	^ data
]

{ #category : 'accessing' }
MCPContentItem >> data: anObject [

	data := anObject
]

{ #category : 'accessing' }
MCPContentItem >> isImage [

	^ type = 'image'
]

{ #category : 'accessing' }
MCPContentItem >> isResource [

	^ type = 'resource'
]

{ #category : 'accessing' }
MCPContentItem >> isText [

	^ type = 'text'
]

{ #category : 'accessing' }
MCPContentItem >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
MCPContentItem >> mimeType: anObject [

	mimeType := anObject
]

{ #category : 'accessing' }
MCPContentItem >> text [

	^ text
]

{ #category : 'accessing' }
MCPContentItem >> text: anObject [

	text := anObject
]

{ #category : 'accessing' }
MCPContentItem >> type [

	^ type
]

{ #category : 'accessing' }
MCPContentItem >> type: anObject [

	type := anObject
]

{ #category : 'accessing' }
MCPContentItem >> uri [

	^ uri
]

{ #category : 'accessing' }
MCPContentItem >> uri: anObject [

	uri := anObject
]
