Class {
	#name : 'McpPrompt',
	#superclass : 'Object',
	#instVars : [
		'name',
		'description',
		'arguments',
		'handler'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'instance creation' }
McpPrompt class >> name: aName description: aDescription arguments: argArray handler: aBlock [

	^ self new
		  name: aName;
		  description: aDescription;
		  arguments: argArray;
		  handler: aBlock;
		  yourself
]

{ #category : 'instance creation' }
McpPrompt class >> name: aName description: aDescription handler: aBlock [

	^ self
		  name: aName
		  description: aDescription
		  arguments: #(  )
		  handler: aBlock
]

{ #category : 'accessing' }
McpPrompt >> name [

	^ name
]

{ #category : 'accessing' }
McpPrompt >> name: anObject [

	name := anObject
]

{ #category : 'accessing' }
McpPrompt >> description [

	^ description
]

{ #category : 'accessing' }
McpPrompt >> description: anObject [

	description := anObject
]

{ #category : 'accessing' }
McpPrompt >> arguments [

	^ arguments
]

{ #category : 'accessing' }
McpPrompt >> arguments: anObject [

	arguments := anObject
]

{ #category : 'accessing' }
McpPrompt >> handler [

	^ handler
]

{ #category : 'accessing' }
McpPrompt >> handler: anObject [

	handler := anObject
]

{ #category : 'executing' }
McpPrompt >> getWithArguments: argDict [
	"Execute the prompt handler with the given arguments.
	 Returns a dictionary with 'description' and 'messages' keys."

	| requiredArgs |
	requiredArgs := arguments select: [ :arg | arg isRequired ].
	requiredArgs do: [ :arg |
		(argDict includesKey: arg name) ifFalse: [
			^ self error:
				  'Missing required argument: ' , arg name ] ].
	[ ^ handler value: argDict ]
		on: Error
		do: [ :ex | ^ self error: ex messageText ]
]

{ #category : 'converting' }
McpPrompt >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'name' put: name.
	description ifNotNil: [ dict at: 'description' put: description ].
	arguments isNotEmpty ifTrue: [
		dict
			at: 'arguments'
			put: (arguments collect: [ :each | each asDictionary ]) ].
	^ dict
]
