"Filed out from Dolphin Smalltalk 7"!

GitHubObject subclass: #GitHubShaObject
	instanceVariableNames: 'sha'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubShaObject guid: (GUID fromString: '{f4a58d8e-ad18-4f89-b328-0ba6e6e24c54}')!
GitHubShaObject comment: ''!
!GitHubShaObject categoriesForClass!Kernel-Objects! !
!GitHubShaObject methodsFor!

children

	^self subclassResponsibility!

initializeFromJson: aDictionary

	super initializeFromJson: aDictionary.

	self
		sha: (aDictionary at: 'sha');
		yourself!

isValid

	^self sha notNil!

repository

	^self subclassResponsibility!

sha
	^sha!

sha: anObject
	sha := anObject! !
!GitHubShaObject categoriesFor: #children!accessing!public! !
!GitHubShaObject categoriesFor: #initializeFromJson:!initialize/release!private! !
!GitHubShaObject categoriesFor: #isValid!public!testing! !
!GitHubShaObject categoriesFor: #repository!accessing!public! !
!GitHubShaObject categoriesFor: #sha!accessing!public! !
!GitHubShaObject categoriesFor: #sha:!accessing!public! !

