Class {
	#name : 'McpSchemaTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests - primitive types' }
McpSchemaTest >> testStringSchema [

	| schema dict |
	schema := McpSchema string.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'string'.
	self deny: (dict includesKey: 'description')
]

{ #category : 'tests - primitive types' }
McpSchemaTest >> testStringWithDescription [

	| schema dict |
	schema := McpSchema string: 'A name value'.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'string'.
	self assert: (dict at: 'description') equals: 'A name value'
]

{ #category : 'tests - primitive types' }
McpSchemaTest >> testNumberSchema [

	| schema dict |
	schema := McpSchema number.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'number'
]

{ #category : 'tests - primitive types' }
McpSchemaTest >> testNumberWithDescription [

	| schema dict |
	schema := McpSchema number: 'Temperature value'.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'number'.
	self assert: (dict at: 'description') equals: 'Temperature value'
]

{ #category : 'tests - primitive types' }
McpSchemaTest >> testIntegerSchema [

	| schema dict |
	schema := McpSchema integer.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'integer'
]

{ #category : 'tests - primitive types' }
McpSchemaTest >> testIntegerWithDescription [

	| schema |
	schema := McpSchema integer: 'Count of items'.
	self assert: schema type equals: 'integer'.
	self assert: schema description equals: 'Count of items'
]

{ #category : 'tests - primitive types' }
McpSchemaTest >> testBooleanSchema [

	| schema dict |
	schema := McpSchema boolean.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'boolean'
]

{ #category : 'tests - primitive types' }
McpSchemaTest >> testBooleanWithDescription [

	| schema |
	schema := McpSchema boolean: 'Is active'.
	self assert: schema type equals: 'boolean'.
	self assert: schema description equals: 'Is active'
]

{ #category : 'tests - complex types' }
McpSchemaTest >> testObjectSchema [

	| schema dict |
	schema := McpSchema object: {
			          'name' -> McpSchema string.
			          'age' -> McpSchema integer }.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'object'.
	self assert: ((dict at: 'properties') includesKey: 'name').
	self assert: ((dict at: 'properties') includesKey: 'age').
	self
		assert: (((dict at: 'properties') at: 'name') at: 'type')
		equals: 'string'.
	self
		assert: (((dict at: 'properties') at: 'age') at: 'type')
		equals: 'integer'
]

{ #category : 'tests - complex types' }
McpSchemaTest >> testObjectWithRequired [

	| schema dict |
	schema := McpSchema
		          object: { 'name' -> McpSchema string.
			          'email' -> McpSchema string }
		          required: #( 'name' 'email' ).
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'object'.
	self assert: (dict at: 'required') equals: #( 'name' 'email' )
]

{ #category : 'tests - complex types' }
McpSchemaTest >> testEmptyObjectSchema [

	| schema dict |
	schema := McpSchema object: {  }.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'object'.
	self assert: (dict at: 'properties') isEmpty
]

{ #category : 'tests - complex types' }
McpSchemaTest >> testArraySchema [

	| schema dict |
	schema := McpSchema array: McpSchema string.
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'array'.
	self assert: ((dict at: 'items') at: 'type') equals: 'string'
]

{ #category : 'tests - complex types' }
McpSchemaTest >> testEnumSchema [

	| schema dict |
	schema := McpSchema enum: #( 'red' 'green' 'blue' ).
	dict := schema asDictionary.
	self assert: (dict at: 'type') equals: 'string'.
	self assert: (dict at: 'enum') equals: #( 'red' 'green' 'blue' )
]

{ #category : 'tests - nested' }
McpSchemaTest >> testNestedObjectSchema [

	| schema dict addressDict |
	schema := McpSchema object: {
			          'name' -> McpSchema string.
			          'address' -> (McpSchema object: {
					           'city' -> McpSchema string.
					           'zip' -> McpSchema string }) }.
	dict := schema asDictionary.
	addressDict := (dict at: 'properties') at: 'address'.
	self assert: (addressDict at: 'type') equals: 'object'.
	self assert: ((addressDict at: 'properties') includesKey: 'city').
	self assert: ((addressDict at: 'properties') includesKey: 'zip')
]
