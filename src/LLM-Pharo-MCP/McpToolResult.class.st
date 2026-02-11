Class {
	#name : 'McpToolResult',
	#superclass : 'Object',
	#instVars : [
		'content',
		'isError'
	],
	#category : 'LLM-Pharo-MCP-Tools',
	#package : 'LLM-Pharo-MCP',
	#tag : 'Tools'
}

{ #category : 'as yet unclassified' }
McpToolResult class >> error: contentItems [

	^ self new
		  content: contentItems;
		  isError: true;
		  yourself
]

{ #category : 'as yet unclassified' }
McpToolResult class >> errorText: aString [

	^ self error: { MCPContentItem text: aString }
]

{ #category : 'instance creation' }
McpToolResult class >> success: contentItems [

	^ self new
		  content: contentItems;
		  isError: false;
		  yourself
]

{ #category : 'as yet unclassified' }
McpToolResult class >> successText: aString [

	^ self success: { MCPContentItem text: aString }
]
