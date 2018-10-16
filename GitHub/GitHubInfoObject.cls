"Filed out from Dolphin Smalltalk 7"!

GitHubObject subclass: #GitHubInfoObject
	instanceVariableNames: 'api infoRequest name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubInfoObject guid: (GUID fromString: '{b9ca570e-3a29-45f3-9dc0-b38a906a1b15}')!
GitHubInfoObject comment: ''!
!GitHubInfoObject categoriesForClass!Kernel-Objects! !
!GitHubInfoObject methodsFor!

api
	^api!

api: anObject
	api := anObject!

flushLocalCaches

	^self!

hasJSON

	"The receiver can lazy-intiialize its own JSON"
	self json.

	^true!

infoRequest
	^infoRequest ifNil: [infoRequest := self newRequest]!

json

	^json ifNil: [self refresh. json]!

name
	^name!

name: anObject
	name := anObject!

newRequest

	^self api newRequest!

refresh

	self infoRequest get.

	self infoRequest isSuccessful 
		ifTrue: [self infoRequest isModified ifTrue: [self initializeFromJson: self infoRequest json; flushLocalCaches]]
		ifFalse: [self json: Dictionary new; flushLocalCaches]! !
!GitHubInfoObject categoriesFor: #api!accessing!private! !
!GitHubInfoObject categoriesFor: #api:!accessing!private! !
!GitHubInfoObject categoriesFor: #flushLocalCaches!operations!private! !
!GitHubInfoObject categoriesFor: #hasJSON!public!testing! !
!GitHubInfoObject categoriesFor: #infoRequest!accessing!private! !
!GitHubInfoObject categoriesFor: #json!accessing!private! !
!GitHubInfoObject categoriesFor: #name!accessing!public! !
!GitHubInfoObject categoriesFor: #name:!accessing!private! !
!GitHubInfoObject categoriesFor: #newRequest!accessing!private! !
!GitHubInfoObject categoriesFor: #refresh!operations!public! !

