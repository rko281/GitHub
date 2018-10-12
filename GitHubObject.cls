"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GitHubObject
	instanceVariableNames: 'json'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubObject guid: (GUID fromString: '{5779341c-6c1d-40c1-b5d8-3e7028d99c5d}')!
GitHubObject comment: ''!
!GitHubObject categoriesForClass!Kernel-Objects! !
!GitHubObject methodsFor!

<= anotherItem

	^self name <= anotherItem name!

api

	^self subclassResponsibility!

displayOn: aStream

	aStream nextPutAll: self name!

hasJSON

	^json notNil!

ifNotValid: aBlock

	^self isValid
		ifTrue: [self]
		ifFalse: [aBlock value]!

ifValid

	^self ifNotValid: [nil]!

initializeFromJson: aDictionary

	self json: aDictionary!

isValid

	^self hasJSON and: [self json includesKey: 'url']!

json
	^json!

json: anObject
	json := anObject!

name

	^self subclassResponsibility!

printOn: aStream

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

url

	"Strip any trailing parameters"
	^(self json lookup: 'url') ifNotNil: [ :urlString | urlString readStream upTo: $?]! !
!GitHubObject categoriesFor: #<=!comparing!public! !
!GitHubObject categoriesFor: #api!accessing!public! !
!GitHubObject categoriesFor: #displayOn:!printing!public! !
!GitHubObject categoriesFor: #hasJSON!public!testing! !
!GitHubObject categoriesFor: #ifNotValid:!accessing!public! !
!GitHubObject categoriesFor: #ifValid!accessing!public! !
!GitHubObject categoriesFor: #initializeFromJson:!initialize/release!private! !
!GitHubObject categoriesFor: #isValid!public!testing! !
!GitHubObject categoriesFor: #json!accessing!private! !
!GitHubObject categoriesFor: #json:!accessing!private! !
!GitHubObject categoriesFor: #name!accessing!public! !
!GitHubObject categoriesFor: #printOn:!printing!public! !
!GitHubObject categoriesFor: #url!accessing!public! !

