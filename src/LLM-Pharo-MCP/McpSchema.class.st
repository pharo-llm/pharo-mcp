Class {
	#name : 'McpSchema',
	#superclass : 'Object',
	#instVars : [
		'type',
		'properties',
		'required',
		'description',
		'items',
		'enum'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'instance creation' }
McpSchema class >> object: propertyAssociations [
	"Create an object schema from an array of property name -> McpSchema associations"

	| props |
	props := OrderedDictionary new.
	propertyAssociations do: [ :assoc | props at: assoc key put: assoc value ].
	^ self new
		  type: 'object';
		  properties: props;
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> object: propertyAssociations required: requiredNames [
	"Create an object schema with required properties"

	^ (self object: propertyAssociations)
		  required: requiredNames;
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> string [

	^ self new
		  type: 'string';
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> string: aDescription [

	^ self new
		  type: 'string';
		  description: aDescription;
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> number [

	^ self new
		  type: 'number';
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> number: aDescription [

	^ self new
		  type: 'number';
		  description: aDescription;
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> integer [

	^ self new
		  type: 'integer';
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> integer: aDescription [

	^ self new
		  type: 'integer';
		  description: aDescription;
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> boolean [

	^ self new
		  type: 'boolean';
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> boolean: aDescription [

	^ self new
		  type: 'boolean';
		  description: aDescription;
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> array: itemSchema [

	^ self new
		  type: 'array';
		  items: itemSchema;
		  yourself
]

{ #category : 'instance creation' }
McpSchema class >> enum: values [

	^ self new
		  type: 'string';
		  enum: values;
		  yourself
]

{ #category : 'accessing' }
McpSchema >> type [

	^ type
]

{ #category : 'accessing' }
McpSchema >> type: anObject [

	type := anObject
]

{ #category : 'accessing' }
McpSchema >> properties [

	^ properties
]

{ #category : 'accessing' }
McpSchema >> properties: anObject [

	properties := anObject
]

{ #category : 'accessing' }
McpSchema >> required [

	^ required
]

{ #category : 'accessing' }
McpSchema >> required: anObject [

	required := anObject
]

{ #category : 'accessing' }
McpSchema >> description [

	^ description
]

{ #category : 'accessing' }
McpSchema >> description: anObject [

	description := anObject
]

{ #category : 'accessing' }
McpSchema >> items [

	^ items
]

{ #category : 'accessing' }
McpSchema >> items: anObject [

	items := anObject
]

{ #category : 'accessing' }
McpSchema >> enum [

	^ enum
]

{ #category : 'accessing' }
McpSchema >> enum: anObject [

	enum := anObject
]

{ #category : 'converting' }
McpSchema >> asDictionary [

	| dict |
	dict := OrderedDictionary new.
	dict at: 'type' put: type.
	description ifNotNil: [ dict at: 'description' put: description ].
	properties ifNotNil: [
		| propsDict |
		propsDict := OrderedDictionary new.
		properties keysAndValuesDo: [ :k :v |
			propsDict at: k put: v asDictionary ].
		dict at: 'properties' put: propsDict ].
	required ifNotNil: [ dict at: 'required' put: required asArray ].
	items ifNotNil: [ dict at: 'items' put: items asDictionary ].
	enum ifNotNil: [ dict at: 'enum' put: enum asArray ].
	^ dict
]
