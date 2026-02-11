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
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
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
McpContentItem >> data [

	^ data
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
