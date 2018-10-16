"Filed out from Dolphin Smalltalk 7"!

Object subclass: #JsonFiler
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JsonFiler guid: (GUID fromString: '{0a6abb10-d5fe-46a6-90e4-547d6d3506f4}')!
JsonFiler comment: ''!
!JsonFiler categoriesForClass!Unclassified! !
!JsonFiler methodsFor!

close
stream close!

stream
	^stream!

stream: anObject
	stream := anObject! !
!JsonFiler categoriesFor: #close!public! !
!JsonFiler categoriesFor: #stream!accessing!public! !
!JsonFiler categoriesFor: #stream:!accessing!private! !

!JsonFiler class methodsFor!

icon
^Icon fromFile: 'Udo Schneider\Goodies\Resources\json.ico'!

on: aStream
^self new stream: aStream; yourself! !
!JsonFiler class categoriesFor: #icon!public! !
!JsonFiler class categoriesFor: #on:!instance/creation!public! !

