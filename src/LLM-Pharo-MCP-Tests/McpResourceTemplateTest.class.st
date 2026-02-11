Class {
	#name : 'McpResourceTemplateTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests - creation' }
McpResourceTemplateTest >> testCreateWithAllFields [

	| template |
	template := McpResourceTemplate
		            uriTemplate: 'pharo://class/{className}'
		            name: 'Class Details'
		            description: 'Get details for a class'
		            mimeType: 'application/json'
		            handler: [ :params | { McpContentItem text: (params at: 'className') } ].
	self assert: template uriTemplate equals: 'pharo://class/{className}'.
	self assert: template name equals: 'Class Details'.
	self assert: template description equals: 'Get details for a class'.
	self assert: template mimeType equals: 'application/json'
]

{ #category : 'tests - creation' }
McpResourceTemplateTest >> testCreateWithDefaults [

	| template |
	template := McpResourceTemplate
		            uriTemplate: 'pharo://method/{className}/{selector}'
		            name: 'Method Source'
		            description: 'Get method source'
		            handler: [ :params | { McpContentItem text: 'source' } ].
	self assert: template mimeType equals: 'text/plain'
]

{ #category : 'tests - matching' }
McpResourceTemplateTest >> testExtractParamsSimple [

	| template params |
	template := McpResourceTemplate new
		            uriTemplate: 'pharo://class/{className}'.
	params := template extractParams: 'pharo://class/Object'.
	self assert: params isNotNil.
	self assert: (params at: 'className') equals: 'Object'
]

{ #category : 'tests - matching' }
McpResourceTemplateTest >> testExtractParamsMultiple [

	| template params |
	template := McpResourceTemplate new
		            uriTemplate: 'pharo://method/{className}/{selector}'.
	params := template extractParams: 'pharo://method/String/size'.
	self assert: params isNotNil.
	self assert: (params at: 'className') equals: 'String'.
	self assert: (params at: 'selector') equals: 'size'
]

{ #category : 'tests - matching' }
McpResourceTemplateTest >> testExtractParamsNoMatch [

	| template params |
	template := McpResourceTemplate new
		            uriTemplate: 'pharo://class/{className}'.
	params := template extractParams: 'pharo://method/Object/size'.
	self assert: params isNil
]

{ #category : 'tests - matching' }
McpResourceTemplateTest >> testExtractParamsDifferentSegmentCount [

	| template params |
	template := McpResourceTemplate new
		            uriTemplate: 'pharo://class/{className}'.
	params := template extractParams: 'pharo://class/Object/extra'.
	self assert: params isNil
]

{ #category : 'tests - matching' }
McpResourceTemplateTest >> testMatchesUri [

	| template |
	template := McpResourceTemplate new
		            uriTemplate: 'pharo://class/{className}'.
	self assert: (template matchesUri: 'pharo://class/Object').
	self deny: (template matchesUri: 'pharo://method/Object')
]

{ #category : 'tests - execution' }
McpResourceTemplateTest >> testReadWithUri [

	| template contents |
	template := McpResourceTemplate
		            uriTemplate: 'pharo://class/{className}'
		            name: 'Class'
		            description: 'Get class'
		            handler: [ :params |
			            { McpContentItem text: 'Class: ' , (params at: 'className') } ].
	contents := template readWithUri: 'pharo://class/SmallInteger'.
	self assert: contents size equals: 1.
	self assert: contents first text equals: 'Class: SmallInteger'
]

{ #category : 'tests - execution' }
McpResourceTemplateTest >> testReadWithUriNoMatch [

	| template contents |
	template := McpResourceTemplate
		            uriTemplate: 'pharo://class/{className}'
		            name: 'Class'
		            description: 'Get class'
		            handler: [ :params | { McpContentItem text: 'ok' } ].
	contents := template readWithUri: 'pharo://wrong/path/here'.
	self assert: contents size equals: 1.
	self assert: (contents first text includesSubstring: 'does not match')
]

{ #category : 'tests - execution' }
McpResourceTemplateTest >> testReadWithUriHandlerError [

	| template contents |
	template := McpResourceTemplate
		            uriTemplate: 'pharo://class/{className}'
		            name: 'Class'
		            description: 'Get class'
		            handler: [ :params | self error: 'Handler failure' ].
	contents := template readWithUri: 'pharo://class/Object'.
	self assert: contents size equals: 1.
	self assert: (contents first text includesSubstring: 'Handler failure')
]

{ #category : 'tests - converting' }
McpResourceTemplateTest >> testAsDictionary [

	| template dict |
	template := McpResourceTemplate
		            uriTemplate: 'pharo://class/{className}'
		            name: 'Class Details'
		            description: 'Get class details'
		            mimeType: 'application/json'
		            handler: [ :p | {  } ].
	dict := template asDictionary.
	self assert: (dict at: 'uriTemplate') equals: 'pharo://class/{className}'.
	self assert: (dict at: 'name') equals: 'Class Details'.
	self assert: (dict at: 'description') equals: 'Get class details'.
	self assert: (dict at: 'mimeType') equals: 'application/json'
]
