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
	#category : 'LLM-Pharo-MCP-Ressources',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Ressources'
}

{ #category : 'instance creation' }
McpSchema class >> number [

	^ self new
		  type: 'number';
		  yourself
]

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
