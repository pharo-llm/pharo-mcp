Class {
	#name : 'McpServerCapabilitiesTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests - defaults' }
McpServerCapabilitiesTest >> testDefaultCapabilities [

	| caps |
	caps := McpServerCapabilities new.
	self assert: caps supportsTools.
	self assert: caps supportsResources.
	self assert: caps supportsPrompts.
	self deny: caps supportsResourceSubscriptions
]

{ #category : 'tests - configuration' }
McpServerCapabilitiesTest >> testDisableTools [

	| caps |
	caps := McpServerCapabilities new.
	caps supportsTools: false.
	self deny: caps supportsTools
]

{ #category : 'tests - configuration' }
McpServerCapabilitiesTest >> testEnableSubscriptions [

	| caps |
	caps := McpServerCapabilities new.
	caps supportsResourceSubscriptions: true.
	self assert: caps supportsResourceSubscriptions
]

{ #category : 'tests - converting' }
McpServerCapabilitiesTest >> testAsDictionaryAllEnabled [

	| caps dict |
	caps := McpServerCapabilities new.
	dict := caps asDictionary.
	self assert: (dict includesKey: 'tools').
	self assert: (dict includesKey: 'resources').
	self assert: (dict includesKey: 'prompts').
	self assert: ((dict at: 'resources') at: 'subscribe') equals: false
]

{ #category : 'tests - converting' }
McpServerCapabilitiesTest >> testAsDictionaryToolsDisabled [

	| caps dict |
	caps := McpServerCapabilities new.
	caps supportsTools: false.
	dict := caps asDictionary.
	self deny: (dict includesKey: 'tools').
	self assert: (dict includesKey: 'resources').
	self assert: (dict includesKey: 'prompts')
]

{ #category : 'tests - converting' }
McpServerCapabilitiesTest >> testAsDictionaryResourcesDisabled [

	| caps dict |
	caps := McpServerCapabilities new.
	caps supportsResources: false.
	dict := caps asDictionary.
	self assert: (dict includesKey: 'tools').
	self deny: (dict includesKey: 'resources')
]

{ #category : 'tests - converting' }
McpServerCapabilitiesTest >> testAsDictionaryPromptsDisabled [

	| caps dict |
	caps := McpServerCapabilities new.
	caps supportsPrompts: false.
	dict := caps asDictionary.
	self deny: (dict includesKey: 'prompts')
]

{ #category : 'tests - converting' }
McpServerCapabilitiesTest >> testAsDictionaryWithSubscriptions [

	| caps dict |
	caps := McpServerCapabilities new.
	caps supportsResourceSubscriptions: true.
	dict := caps asDictionary.
	self assert: ((dict at: 'resources') at: 'subscribe') equals: true
]
