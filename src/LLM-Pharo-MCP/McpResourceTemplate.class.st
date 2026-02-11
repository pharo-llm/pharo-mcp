Class {
	#name : 'McpResourceTemplate',
	#superclass : 'Object',
	#instVars : [
		'uriTemplate',
		'name',
		'description',
		'mimeType',
		'handler'
	],
	#category : 'LLM-Pharo-MCP-Ressources',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Ressources'
}

{ #category : 'instance creation' }
McpResourceTemplate class >> uriTemplate: aTemplate name: aName description: aDescription handler: aBlock [

	^ self
		  uriTemplate: aTemplate
		  name: aName
		  description: aDescription
		  mimeType: 'text/plain'
		  handler: aBlock
]

{ #category : 'instance creation' }
McpResourceTemplate class >> uriTemplate: aTemplate name: aName description: aDescription mimeType: aMimeType handler: aBlock [

	^ self new
		  uriTemplate: aTemplate;
		  name: aName;
		  description: aDescription;
		  mimeType: aMimeType;
		  handler: aBlock;
		  yourself
]

{ #category : 'converting' }
McpResourceTemplate >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'uriTemplate' put: uriTemplate.
	dict at: 'name' put: name.
	description ifNotNil: [ dict at: 'description' put: description ].
	mimeType ifNotNil: [ dict at: 'mimeType' put: mimeType ].
	^ dict
]

{ #category : 'accessing' }
McpResourceTemplate >> description [

	^ description
]

{ #category : 'accessing' }
McpResourceTemplate >> description: anObject [

	description := anObject
]

{ #category : 'matching' }
McpResourceTemplate >> extractParams: aUri [
	"Extract parameter values from a URI that matches this template.
	 Returns a dictionary of param name -> value, or nil if no match."

	| templateParts uriParts params |
	templateParts := $/ split: uriTemplate.
	uriParts := $/ split: aUri.
	templateParts size = uriParts size ifFalse: [ ^ nil ].
	params := Dictionary new.
	templateParts with: uriParts do: [ :tPart :uPart |
		(tPart beginsWith: '{')
			ifTrue: [
				| paramName |
				paramName := tPart copyFrom: 2 to: tPart size - 1.
				params at: paramName put: uPart ]
			ifFalse: [ tPart = uPart ifFalse: [ ^ nil ] ] ].
	^ params
]

{ #category : 'accessing' }
McpResourceTemplate >> handler [

	^ handler
]

{ #category : 'accessing' }
McpResourceTemplate >> handler: anObject [

	handler := anObject
]

{ #category : 'matching' }
McpResourceTemplate >> matchesUri: aUri [
	"Check if the given URI matches this template pattern.
	 Template uses {paramName} syntax for variable parts."

	| pattern |
	pattern := uriTemplate copyReplaceAll: '\{' with: '('.
	pattern := pattern copyReplaceAll: '\}' with: ')'.
	"Simple matching: replace {param} with regex-like wildcard"
	^ (self extractParams: aUri) isNotNil
]

{ #category : 'accessing' }
McpResourceTemplate >> mimeType [

	^ mimeType
]

{ #category : 'accessing' }
McpResourceTemplate >> mimeType: anObject [

	mimeType := anObject
]

{ #category : 'accessing' }
McpResourceTemplate >> name [

	^ name
]

{ #category : 'accessing' }
McpResourceTemplate >> name: anObject [

	name := anObject
]

{ #category : 'executing' }
McpResourceTemplate >> readWithUri: aUri [
	"Read this resource template with extracted parameters"

	| params |
	params := self extractParams: aUri.
	params ifNil: [
		^ { MCPContentItem text: 'URI does not match template' } ].
	[ ^ handler value: params ]
		on: Error
		do: [ :ex |
			^ { MCPContentItem text:
					  'Error reading resource: ' , ex messageText } ]
]

{ #category : 'accessing' }
McpResourceTemplate >> uriTemplate [

	^ uriTemplate
]

{ #category : 'accessing' }
McpResourceTemplate >> uriTemplate: anObject [

	uriTemplate := anObject
]
