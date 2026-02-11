Class {
	#name : 'McpPromptMessage',
	#superclass : 'Object',
	#instVars : [
		'role',
		'content'
	],
	#category : 'LLM-Pharo-MCP-Prompt',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Prompt'
}

{ #category : 'instance creation' }
McpPromptMessage class >> assistant: aString [

	^ self role: 'assistant' content: (MCPContentItem text: aString)
]

{ #category : 'instance creation' }
McpPromptMessage class >> role: aRole content: aContentItem [

	^ self new
		  role: aRole;
		  content: aContentItem;
		  yourself
]

{ #category : 'instance creation' }
McpPromptMessage class >> user: aString [

	^ self role: 'user' content: (MCPContentItem text: aString)
]

{ #category : 'converting' }
McpPromptMessage >> asDictionary [

	^ OrderedDictionary new
		  at: 'role' put: role;
		  at: 'content' put: content asDictionary;
		  yourself
]

{ #category : 'accessing' }
McpPromptMessage >> content [

	^ content
]

{ #category : 'accessing' }
McpPromptMessage >> content: anObject [

	content := anObject
]

{ #category : 'accessing' }
McpPromptMessage >> role [

	^ role
]

{ #category : 'accessing' }
McpPromptMessage >> role: anObject [

	role := anObject
]
