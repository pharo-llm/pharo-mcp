# Resources Guide

Resources expose data from the Pharo environment that an LLM can read. Unlike tools (which perform actions), resources provide context and information.

## Static Resources

A static resource has a fixed URI and returns content when read.

### Creating a Resource

```smalltalk
resource := McpResource
  uri: 'pharo://system/version'
  name: 'System Version'
  description: 'The current Pharo system version'
  mimeType: 'text/plain'
  handler: [ { McpContentItem text: SystemVersion current versionString } ].
```

The handler block takes no arguments and must return an array of `McpContentItem` objects.

### Registering Resources

```smalltalk
server addResource: resource.
```

### Removing Resources

```smalltalk
server removeResource: 'pharo://system/version'.
```

### Default MIME Type

If you omit the MIME type, it defaults to `text/plain`:

```smalltalk
resource := McpResource
  uri: 'pharo://packages'
  name: 'Loaded Packages'
  description: 'List of all loaded packages'
  handler: [
    | names |
    names := RPackageOrganizer default packages collect: #name.
    { McpContentItem text: (String lf join: names sorted) } ].
```

### Multiple Content Items

A resource can return multiple content items:

```smalltalk
resource := McpResource
  uri: 'pharo://system/summary'
  name: 'System Summary'
  description: 'Summary of the Pharo system'
  handler: [
    {
      McpContentItem text: 'Version: ' , SystemVersion current versionString.
      McpContentItem text: 'Classes: ' , Smalltalk globals allClasses size printString.
      McpContentItem text: 'Methods: ' , CompiledMethod allInstances size printString
    } ].
```

## Resource Templates

Resource templates use URI patterns with `{param}` placeholders to serve dynamic content.

### Creating a Template

```smalltalk
template := McpResourceTemplate
  uriTemplate: 'pharo://class/{className}'
  name: 'Class Details'
  description: 'Get details about a Pharo class'
  mimeType: 'application/json'
  handler: [ :params |
    | cls |
    cls := Smalltalk globals at: (params at: 'className') asSymbol ifAbsent: [ nil ].
    cls
      ifNil: [ { McpContentItem text: 'Class not found' } ]
      ifNotNil: [
        { McpContentItem text: (STONJSON toString: (Dictionary new
            at: 'name' put: cls name;
            at: 'superclass' put: cls superclass name;
            at: 'instanceVariables' put: cls instVarNames;
            at: 'methods' put: cls selectors size;
            yourself)) } ] ].
```

### Templates with Multiple Parameters

```smalltalk
template := McpResourceTemplate
  uriTemplate: 'pharo://method/{className}/{selector}'
  name: 'Method Source'
  description: 'Source code of a method'
  handler: [ :params |
    | cls |
    cls := Smalltalk globals at: (params at: 'className') asSymbol ifAbsent: [ nil ].
    cls
      ifNil: [ { McpContentItem text: 'Class not found' } ]
      ifNotNil: [
        | method |
        method := cls lookupSelector: (params at: 'selector') asSymbol.
        method
          ifNil: [ { McpContentItem text: 'Method not found' } ]
          ifNotNil: [ { McpContentItem text: method sourceCode } ] ] ].
```

### Registering Templates

```smalltalk
server addResourceTemplate: template.
```

### How Template Matching Works

When a `resources/read` request arrives, the server:

1. First tries an exact URI match against registered static resources
2. If no match, iterates over resource templates and tries to match the URI
3. If a template matches, extracts parameters and calls the handler

The matching works by splitting the URI template and the actual URI by `/` and comparing segments. Segments starting with `{` are treated as parameters.

Example:
- Template: `pharo://class/{className}`
- URI: `pharo://class/OrderedCollection`
- Extracted params: `{ 'className' -> 'OrderedCollection' }`

## Error Handling

Both resources and resource templates automatically catch errors from handler blocks:

```smalltalk
resource := McpResource
  uri: 'pharo://risky'
  name: 'Risky Resource'
  description: 'Might fail'
  handler: [
    "If this raises an error, the resource returns an error content item"
    self error: 'Something went wrong' ].
```

The error is returned as a content item with text `"Error reading resource: Something went wrong"` rather than crashing the server.

## URI Conventions

We recommend using `pharo://` as the URI scheme for Pharo-related resources:

| URI Pattern | Description |
|-------------|-------------|
| `pharo://system/version` | System version |
| `pharo://system/info` | System information |
| `pharo://packages` | List of packages |
| `pharo://class/{className}` | Class details |
| `pharo://method/{className}/{selector}` | Method source |
| `pharo://package/{packageName}` | Package details |
