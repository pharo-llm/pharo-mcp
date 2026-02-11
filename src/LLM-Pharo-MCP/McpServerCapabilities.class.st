Class {
	#name : 'McpServerCapabilities',
	#superclass : 'Object',
	#instVars : [
		'supportsTools',
		'supportsResources',
		'supportsPrompts',
		'supportsResourceSubscriptions'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'initialization' }
McpServerCapabilities >> initialize [

	super initialize.
	supportsTools := true.
	supportsResources := true.
	supportsPrompts := true.
	supportsResourceSubscriptions := false
]

{ #category : 'accessing' }
McpServerCapabilities >> supportsTools [

	^ supportsTools
]

{ #category : 'accessing' }
McpServerCapabilities >> supportsTools: anObject [

	supportsTools := anObject
]

{ #category : 'accessing' }
McpServerCapabilities >> supportsResources [

	^ supportsResources
]

{ #category : 'accessing' }
McpServerCapabilities >> supportsResources: anObject [

	supportsResources := anObject
]

{ #category : 'accessing' }
McpServerCapabilities >> supportsPrompts [

	^ supportsPrompts
]

{ #category : 'accessing' }
McpServerCapabilities >> supportsPrompts: anObject [

	supportsPrompts := anObject
]

{ #category : 'accessing' }
McpServerCapabilities >> supportsResourceSubscriptions [

	^ supportsResourceSubscriptions
]

{ #category : 'accessing' }
McpServerCapabilities >> supportsResourceSubscriptions: anObject [

	supportsResourceSubscriptions := anObject
]

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
