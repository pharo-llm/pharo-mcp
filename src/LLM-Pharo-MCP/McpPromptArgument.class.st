Class {
	#name : 'McpPromptArgument',
	#superclass : 'Object',
	#instVars : [
		'name',
		'description',
		'required'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'instance creation' }
McpPromptArgument class >> name: aName description: aDescription required: aBoolean [

	^ self new
		  name: aName;
		  description: aDescription;
		  required: aBoolean;
		  yourself
]

{ #category : 'instance creation' }
McpPromptArgument class >> name: aName description: aDescription [

	^ self name: aName description: aDescription required: false
]

{ #category : 'instance creation' }
McpPromptArgument class >> required: aName description: aDescription [
	"Convenience for creating a required argument"

	^ self name: aName description: aDescription required: true
]

{ #category : 'accessing' }
McpPromptArgument >> name [

	^ name
]

{ #category : 'accessing' }
McpPromptArgument >> name: anObject [

	name := anObject
]

{ #category : 'accessing' }
McpPromptArgument >> description [

	^ description
]

{ #category : 'accessing' }
McpPromptArgument >> description: anObject [

	description := anObject
]

{ #category : 'accessing' }
McpPromptArgument >> required [

	^ required
]

{ #category : 'accessing' }
McpPromptArgument >> required: anObject [

	required := anObject
]

{ #category : 'testing' }
McpPromptArgument >> isRequired [

	^ required
]

{ #category : 'converting' }
McpPromptArgument >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'name' put: name.
	description ifNotNil: [ dict at: 'description' put: description ].
	dict at: 'required' put: required.
	^ dict
]
