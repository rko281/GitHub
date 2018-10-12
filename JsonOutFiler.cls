"Filed out from Dolphin Smalltalk 7"!

JsonFiler subclass: #JsonOutFiler
	instanceVariableNames: ''
	classVariableNames: 'CharacterEscapeMap'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JsonOutFiler guid: (GUID fromString: '{f467bea0-3fa5-4d82-bedf-855bfc4af696}')!
JsonOutFiler comment: ''!
!JsonOutFiler categoriesForClass!Unclassified! !
!JsonOutFiler methodsFor!

jsonSaveOn: aJsonOutFiler
aJsonOutFiler saveNil!

nextPut: anObject
anObject jsonSaveOn: self!

saveArray: anArray 
	stream nextPut: $[.
	anArray do: [:each | each jsonSaveOn: self]
		separatedBy: 
			[stream
				nextPut: $,].
	stream nextPut: $]!

saveBoolean: aBoolean
stream nextPutAll: (aBoolean  ifTrue: ['true'] ifFalse: ['false'] )!

saveColor: aColor 
	| colorString |
	colorString := '#' , ('0' , (aColor red printStringRadix: 16 showRadix: false) last: 2) 
				, ('0' , (aColor green printStringRadix: 16 showRadix: false) last: 2) 
					, ('0' , (aColor blue printStringRadix: 16 showRadix: false) last: 2).
					self saveString: colorString!

saveDictionary: aDictionary 
	stream nextPut: ${.
	aDictionary associationsDo: 
			[:association | 
			association key jsonSaveOn: self.
			stream nextPut: $:.
			association value jsonSaveOn: self]
		separatedBy: 
			[stream
				nextPut: $,].
	stream nextPut: $}!

saveFloat: aFloat 
	aFloat printOn: stream decimalPlaces: 2!

saveNil
stream nextPutAll: 'null'!

saveNumber: aNumber 
	stream nextPutAll: aNumber  displayString!

savePoint: aPoint


	self saveDictionary: ( (Dictionary new
		at: #x
			put: aPoint x;
		at: #y
			put: aPoint y;
		yourself) )!

saveString: aString 
	stream nextPut: $".
	aString  do: 
			[:each | 
			(CharacterEscapeMap at: each ifAbsent: [nil]) 
				ifNotNil: [:value | stream nextPutAll: value]
				ifNil: [stream nextPut: each]].
	stream nextPut: $"! !
!JsonOutFiler categoriesFor: #jsonSaveOn:!public! !
!JsonOutFiler categoriesFor: #nextPut:!public! !
!JsonOutFiler categoriesFor: #saveArray:!private! !
!JsonOutFiler categoriesFor: #saveBoolean:!private! !
!JsonOutFiler categoriesFor: #saveColor:!private! !
!JsonOutFiler categoriesFor: #saveDictionary:!private! !
!JsonOutFiler categoriesFor: #saveFloat:!private! !
!JsonOutFiler categoriesFor: #saveNil!private! !
!JsonOutFiler categoriesFor: #saveNumber:!private! !
!JsonOutFiler categoriesFor: #savePoint:!private! !
!JsonOutFiler categoriesFor: #saveString:!private! !

!JsonOutFiler class methodsFor!

initialize
	"
	self initialize
	"

	CharacterEscapeMap := Dictionary new.
	CharacterEscapeMap
		at: $" put: '\"';
		at: $\ put: '\\';
		at: Character backspace put: '\b';
		at: Character ff put: '\f';
		at: Character nl put: '\n';
		at: Character cr put: '\r';
		at: Character tab put: '\t'.
	"at: $/ put: '\/';"
	^super initialize! !
!JsonOutFiler class categoriesFor: #initialize!public! !

