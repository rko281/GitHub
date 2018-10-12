"Filed out from Dolphin Smalltalk 7"!

Object subclass: #ShadowClass
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShadowClass guid: (GUID fromString: '{06a4002d-93e3-44a4-ae35-89143cfe16dc}')!
ShadowClass comment: ''!
!ShadowClass categoriesForClass!Kernel-Objects! !
!ShadowClass methodsFor!

<= aClass

	^self name <= aClass name!

displayOn: aStream
	"Append, to aStream, a String whose characters are a representation of the receiver as a user
	would want to see it."

	aStream nextPutAll: self name
!

name
	^name!

name: anObject
	name := anObject!

printOn: aStream
	"Append a representation of the receiver to aStream."

	aStream
		basicPrint: self;
		nextPut: $(;
		print: self name;
		nextPut: $)
! !
!ShadowClass categoriesFor: #<=!comparing!public! !
!ShadowClass categoriesFor: #displayOn:!displaying!public! !
!ShadowClass categoriesFor: #name!accessing!private! !
!ShadowClass categoriesFor: #name:!accessing!private! !
!ShadowClass categoriesFor: #printOn:!printing!public! !

