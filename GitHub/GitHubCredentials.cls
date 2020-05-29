"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GitHubCredentials
	instanceVariableNames: 'encryptedUsername encryptedToken'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!
GitHubCredentials guid: (GUID fromString: '{bdedbe3f-401b-4af1-86d7-369615dcc102}')!
GitHubCredentials comment: ''!
!GitHubCredentials categoriesForClass!Kernel-Objects! !
!GitHubCredentials methodsFor!

addRequestHeadersTo: aRESTRequest

	aRESTRequest 
		addRequestHeader: 'User-Agent' value: self username;
		addRequestHeader: 'Authorization' value: 'Basic ', (self username, ':', self token) asGitHubBase64!

authorizeWithPassword: password description: aString

	"Private - Using the supplier password and stored username, request a new Authorization token and store in the receiver credentials.

	NB - DO NOT CALL THIS METHOD BY EVALUATING AN EXPRESSION WITH HARD-CODED PARAMS - THIS WILL RECORD THE PASSWORD IN THE CHANGES FILE!!!!!!" 

	| request result |

	request := (RESTRequest url: GitHubAPI url), #('authorizations').
	request 
		addRequestHeader: 'Accept' value: GitHubAPI mediaType;
		addRequestHeader: 'User-Agent' value: self username;
		addRequestHeader: 'Authorization' value: 'Basic ', (self username, ':', password) asGitHubBase64;
		content: 
			(STON toJsonString: 
				(Dictionary new
					at: 'note' put: aString;
					at: 'scopes' put: #('public_repo');
					at: 'fingerprint' put: GUID newUnique displayString;
					yourself)).

	result := request post.
	result isSuccessful ifFalse: [self error: 'authorization failed'].

	self token: (result json at: 'token')!

encryptedToken

	"encryptedToken stores the authorization token encrypted with Crypt32.
	This encrypts the token for the current user, thus any code running under the current user's account can also decrypt this"

	^encryptedToken!

encryptedToken: anObject

	"encryptedToken stores the authorization token encrypted with Crypt32.
	This encrypts the token for the current user, thus any code running under the current user's account can also decrypt this"

	encryptedToken := anObject!

encryptedUsername

	"encryptedUsername stores the username encrypted with Crypt32.
	This encrypts the token for the current user, thus any code running under the current user's account can also decrypt this"

	^encryptedUsername!

encryptedUsername: aString

	"encryptedUsername stores the username encrypted with Crypt32.
	This encrypts the token for the current user, thus any code running under the current user's account can also decrypt this"

	encryptedUsername := aString!

hasToken

	^self encryptedToken notNil!

hasUsername

	^self encryptedUsername notNil!

isComplete

	^self hasUsername and: [self hasToken]!

reset

	self
		username: nil;
		encryptedToken: nil!

token

	^Utf8String fromByteArray: (Crypt32Library unprotectData: self encryptedToken)!

token: aString

	aString isNil
		ifTrue: [self encryptedToken: nil]
		ifFalse: [self encryptedToken: (Crypt32Library protectData: aString asUtf8String asByteArray)]!

username

	^Utf8String fromByteArray: (Crypt32Library unprotectData: self encryptedUsername)!

username: aString

	aString isNil
		ifTrue: [self encryptedUsername: nil]
		ifFalse: [self encryptedUsername: (Crypt32Library protectData: aString asUtf8String asByteArray)]! !
!GitHubCredentials categoriesFor: #addRequestHeadersTo:!operations!private! !
!GitHubCredentials categoriesFor: #authorizeWithPassword:description:!operations!private! !
!GitHubCredentials categoriesFor: #encryptedToken!accessing!private! !
!GitHubCredentials categoriesFor: #encryptedToken:!accessing!private! !
!GitHubCredentials categoriesFor: #encryptedUsername!accessing!private! !
!GitHubCredentials categoriesFor: #encryptedUsername:!accessing!private! !
!GitHubCredentials categoriesFor: #hasToken!public!testing! !
!GitHubCredentials categoriesFor: #hasUsername!public!testing! !
!GitHubCredentials categoriesFor: #isComplete!public!testing! !
!GitHubCredentials categoriesFor: #reset!operations!private! !
!GitHubCredentials categoriesFor: #token!accessing!public! !
!GitHubCredentials categoriesFor: #token:!accessing!public! !
!GitHubCredentials categoriesFor: #username!accessing!public! !
!GitHubCredentials categoriesFor: #username:!accessing!public! !

!GitHubCredentials class methodsFor!

default

	^default ifNil: [default := self new]!

reset

	default := nil!

uninitialize

	self reset! !
!GitHubCredentials class categoriesFor: #default!accessing!public! !
!GitHubCredentials class categoriesFor: #reset!class initialization!private! !
!GitHubCredentials class categoriesFor: #uninitialize!class initialization!private! !

