Class {
	#name : 'McpStdioTransport',
	#superclass : 'McpTransport',
	#instVars : [
		'inputStream',
		'outputStream',
		'running',
		'readProcess'
	],
	#category : 'LLM-Pharo-MCP',
	#package : 'LLM-Pharo-MCP'
}

{ #category : 'instance creation' }
McpStdioTransport class >> stdin: anInputStream stdout: anOutputStream [

	^ self new
		  inputStream: anInputStream;
		  outputStream: anOutputStream;
		  yourself
]

{ #category : 'initialization' }
McpStdioTransport >> initialize [

	super initialize.
	running := false
]

{ #category : 'accessing' }
McpStdioTransport >> inputStream [

	^ inputStream
]

{ #category : 'accessing' }
McpStdioTransport >> inputStream: anObject [

	inputStream := anObject
]

{ #category : 'accessing' }
McpStdioTransport >> outputStream [

	^ outputStream
]

{ #category : 'accessing' }
McpStdioTransport >> outputStream: anObject [

	outputStream := anObject
]

{ #category : 'testing' }
McpStdioTransport >> isRunning [

	^ running
]

{ #category : 'operations' }
McpStdioTransport >> start [
	"Start reading from stdin line by line"

	running := true.
	readProcess := [
	                [ running ] whileTrue: [
		                | line |
		                line := [ inputStream nextLine ]
			                        on: Error
			                        do: [ :ex | nil ].
		                (line isNotNil and: [ line isNotEmpty ]) ifTrue: [
			                messageHandler ifNotNil: [
				                messageHandler value: line ] ] ] ]
		                fork
]

{ #category : 'operations' }
McpStdioTransport >> stop [

	running := false.
	readProcess ifNotNil: [
		readProcess terminate.
		readProcess := nil ]
]

{ #category : 'operations' }
McpStdioTransport >> sendMessage: aJsonString [
	"Write a JSON-RPC message to stdout followed by a newline"

	outputStream
		nextPutAll: aJsonString;
		nextPut: Character lf;
		flush
]
