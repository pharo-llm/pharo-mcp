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

{ #category : 'accessing' }
McpPromptArgument >> name [

	^ name
]
