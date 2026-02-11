Class {
	#name : 'BaselineOfLLMPharoMCP',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfLLMPharoMCP',
	#package : 'BaselineOfLLMPharoMCP'
}

{ #category : 'baselines' }
BaselineOfLLMPharoMCP >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec
			package: 'LLM-Pharo-MCP';
			package: 'LLM-Pharo-MCP-Tests'
			with: [ spec requires: #( 'LLM-Pharo-MCP' ) ].
		spec
			group: 'Core' with: #( 'LLM-Pharo-MCP' );
			group: 'Tests' with: #( 'LLM-Pharo-MCP-Tests' );
			group: 'default' with: #( 'Core' 'Tests' ) ]
]
