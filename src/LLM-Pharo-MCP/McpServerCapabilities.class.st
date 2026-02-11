Class {
	#name : 'McpServerCapabilities',
	#superclass : 'Object',
	#instVars : [
		'supportsTools',
		'supportsResources',
		'supportsPrompts',
		'supportsResourceSubscriptions'
	],
	#category : 'LLM-Pharo-MCP-Server',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Server'
}

{ #category : 'converting' }
McpServerCapabilities >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	supportsTools ifTrue: [
		dict at: 'tools' put: (OrderedDictionary new
				 at: 'listChanged' put: false;
				 yourself) ].
	supportsResources ifTrue: [
		dict at: 'resources' put: (OrderedDictionary new
				 at: 'subscribe' put: supportsResourceSubscriptions;
				 at: 'listChanged' put: false;
				 yourself) ].
	supportsPrompts ifTrue: [
		dict at: 'prompts' put: (OrderedDictionary new
				 at: 'listChanged' put: false;
				 yourself) ].
	^ dict
]

{ #category : 'initialization' }
McpServerCapabilities >> initialize [

	super initialize.
	supportsTools := true.
	supportsResources := true.
	supportsPrompts := true.
	supportsResourceSubscriptions := false
]

{ #category : 'initialization' }
McpServerCapabilities >> supportsPrompts [

	^ supportsPrompts
]

{ #category : 'initialization' }
McpServerCapabilities >> supportsPrompts: anObject [

	supportsPrompts := anObject
]

{ #category : 'initialization' }
McpServerCapabilities >> supportsResourceSubscriptions [

	^ supportsResourceSubscriptions
]

{ #category : 'initialization' }
McpServerCapabilities >> supportsResourceSubscriptions: anObject [

	supportsResourceSubscriptions := anObject
]

{ #category : 'initialization' }
McpServerCapabilities >> supportsResources [

	^ supportsResources
]

{ #category : 'initialization' }
McpServerCapabilities >> supportsResources: anObject [

	supportsResources := anObject
]

{ #category : 'initialization' }
McpServerCapabilities >> supportsTools [

	^ supportsTools
]

{ #category : 'initialization' }
McpServerCapabilities >> supportsTools: anObject [

	supportsTools := anObject
]
