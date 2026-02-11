Class {
	#name : 'McpPromptArgument',
	#superclass : 'Object',
	#instVars : [
		'name',
		'description',
		'required'
	],
	#category : 'LLM-Pharo-MCP-Prompt',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Prompt'
}

{ #category : 'as yet unclassified' }
McpPromptArgument class >> name: aName description: aDescription [

	^ self name: aName description: aDescription required: false
]

{ #category : 'instance creation' }
McpPromptArgument class >> name: aName description: aDescription required: aBoolean [

	^ self new
		  name: aName;
		  description: aDescription;
		  required: aBoolean;
		  yourself
]

{ #category : 'as yet unclassified' }
McpPromptArgument class >> required: aName description: aDescription [
	"Convenience for creating a required argument"

	^ self name: aName description: aDescription required: true
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

{ #category : 'accessing' }
McpPromptArgument >> description [

	^ description
]

{ #category : 'accessing' }
McpPromptArgument >> description: anObject [

	description := anObject
]

{ #category : 'testing' }
McpPromptArgument >> isRequired [

	^ required
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
McpPromptArgument >> required [

	^ required
]

{ #category : 'accessing' }
McpPromptArgument >> required: anObject [

	required := anObject
]
