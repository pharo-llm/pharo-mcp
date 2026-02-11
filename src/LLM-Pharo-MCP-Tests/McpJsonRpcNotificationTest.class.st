Class {
	#name : 'McpJsonRpcNotificationTest',
	#superclass : 'TestCase',
	#category : 'LLM-Pharo-MCP-Tests',
	#package : 'LLM-Pharo-MCP-Tests'
}

{ #category : 'tests - creation' }
McpJsonRpcNotificationTest >> testCreateWithMethod [

	| notif |
	notif := McpJsonRpcNotification method: 'notifications/initialized'.
	self assert: notif method equals: 'notifications/initialized'.
	self assert: notif params isNil.
	self assert: notif jsonrpc equals: '2.0'
]

{ #category : 'tests - creation' }
McpJsonRpcNotificationTest >> testCreateWithMethodAndParams [

	| notif params |
	params := Dictionary new at: 'key' put: 'value'; yourself.
	notif := McpJsonRpcNotification method: 'notifications/update' params: params.
	self assert: notif method equals: 'notifications/update'.
	self assert: (notif params at: 'key') equals: 'value'
]

{ #category : 'tests - testing' }
McpJsonRpcNotificationTest >> testIsNotification [

	| notif |
	notif := McpJsonRpcNotification method: 'notifications/initialized'.
	self assert: notif isNotification
]

{ #category : 'tests - converting' }
McpJsonRpcNotificationTest >> testAsDictionary [

	| notif dict |
	notif := McpJsonRpcNotification method: 'notifications/initialized'.
	dict := notif asDictionary.
	self assert: (dict at: 'jsonrpc') equals: '2.0'.
	self assert: (dict at: 'method') equals: 'notifications/initialized'.
	self deny: (dict includesKey: 'params').
	self deny: (dict includesKey: 'id')
]

{ #category : 'tests - converting' }
McpJsonRpcNotificationTest >> testAsDictionaryWithParams [

	| notif dict params |
	params := Dictionary new at: 'data' put: 42; yourself.
	notif := McpJsonRpcNotification method: 'test' params: params.
	dict := notif asDictionary.
	self assert: ((dict at: 'params') at: 'data') equals: 42
]

{ #category : 'tests - converting' }
McpJsonRpcNotificationTest >> testAsJsonString [

	| notif json parsed |
	notif := McpJsonRpcNotification method: 'notifications/initialized'.
	json := notif asJsonString.
	parsed := STONJSON fromString: json.
	self assert: (parsed at: 'method') equals: 'notifications/initialized'.
	self deny: (parsed includesKey: 'id')
]
