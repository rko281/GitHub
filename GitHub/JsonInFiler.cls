"Filed out from Dolphin Smalltalk 7"!

JsonFiler subclass: #JsonInFiler
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JsonInFiler guid: (GUID fromString: '{7d5a0c0f-5b59-4370-b39c-6f27b19f3f2a}')!
JsonInFiler comment: ''!
!JsonInFiler categoriesForClass!Unclassified! !
!JsonInFiler methodsFor!

next
	| char |
	self skipCommentsAndWhitespace.
	char := stream peek.
	char = $" ifTrue: [^self parseString].
	('-0123456789' includes: char) ifTrue: [^self parseNumber].
	char = $[ ifTrue: [^self parseArray].
	char = $t ifTrue: [^self parseTrue].
	char = $f ifTrue: [^self parseFalse].
	char = $n ifTrue: [^self parseNull].
	char = ${ ifTrue: [^self parseObject].
	self parserError: 'Unknown character sequence found'!

parseArray
	| array |
	array := OrderedCollection new.
	stream next.	"Consume initial ["
	self skipCommentsAndWhitespace.
	[stream atEnd or: [stream peek = $]]] whileFalse: 
			[array add: self next.
			stream peek = $, ifTrue: [stream next].
			self skipCommentsAndWhitespace].
	stream next.
	^array asArray!

parseEscapeCharacter
	| char |
	char := stream next.
	char = $" ifTrue: [^$"].
	char = $\ ifTrue: [^$\].
	char = $/ ifTrue: [^$/].
	char = $b ifTrue: [^Character backspace].
	char = $f ifTrue: [^Character ff].
	char = $n ifTrue: [^Character nl].
	char = $r ifTrue: [^Character cr].
	char = $t ifTrue: [^Character tab].
	char = $u 
		ifTrue: 
			[| hexValue |
			hexValue := Number fromString: '16r' , (stream next: 4) asUppercase.
			^Character value: hexValue.
			].
	Error notYetImplemented!

parseFalse
	stream next: 5.
	^false!

parseNull
stream next: 4.
^nil!

parseNumber
	^Number readSmalltalkSyntaxFrom: stream!

parseObject
	| object |
	object := Dictionary new.
	stream next.	"Consume initial {"
	self skipCommentsAndWhitespace.
	[stream atEnd or: [stream peek = $}]] whileFalse: 
			[| key value |
			key := self next.
			self skipCommentsAndWhitespace.
			stream next = $: ifFalse: [self parserError: 'Colon $: expected but not found'].
			value := self next.
			object at: key put: value.
			self skipCommentsAndWhitespace.
			stream peek = $, ifTrue: [stream next].
			self skipCommentsAndWhitespace].
	self skipCommentsAndWhitespace.
	stream next.
	^object!

parserError: aString 
	JsonError signal: 'Parser Error: ' , aString!

parseString
	| stringStream char |
	stringStream := ReadWriteStream on: String new.
	stream next.	"Consume start double quote"
	[stream atEnd or: [(char := stream next) = $"]] whileFalse: 
			[char = $\ 
				ifFalse: [stringStream nextPut: char]
				ifTrue: [stringStream nextPut: self parseEscapeCharacter]].
	^stringStream contents!

parseTrue
	stream next: 4.
	^true!

skipCommentsAndWhitespace
	| abort |
	abort := false.
	[abort or: [stream atEnd]] whileFalse: 
			[stream peek isWhitespace 
				ifTrue: [stream next]
				ifFalse: [(stream peek: 2) = '/*' ifTrue: [
					stream upToAll: '*/'
					] ifFalse: [abort := true]]]! !
!JsonInFiler categoriesFor: #next!public! !
!JsonInFiler categoriesFor: #parseArray!public! !
!JsonInFiler categoriesFor: #parseEscapeCharacter!public! !
!JsonInFiler categoriesFor: #parseFalse!public! !
!JsonInFiler categoriesFor: #parseNull!public! !
!JsonInFiler categoriesFor: #parseNumber!public! !
!JsonInFiler categoriesFor: #parseObject!public! !
!JsonInFiler categoriesFor: #parserError:!public! !
!JsonInFiler categoriesFor: #parseString!public! !
!JsonInFiler categoriesFor: #parseTrue!public! !
!JsonInFiler categoriesFor: #skipCommentsAndWhitespace!public! !

