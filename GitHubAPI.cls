"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GitHubAPI
	instanceVariableNames: 'credentials owners repositories rateLimit rateLimitRemaining'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubAPI guid: (GUID fromString: '{95bf3664-9e1c-4c08-afef-17eebae9925a}')!
GitHubAPI comment: ''!
!GitHubAPI categoriesForClass!Kernel-Objects! !
!GitHubAPI methodsFor!

_ownerNamed: aString

	^self owners at: aString ifAbsentPut: 
		[GitHubOwner new 
			name: aString;
			api: self;
			yourself]!

canAuthenticate

	^self hasCredentials and: [self newRequest get isSuccessful]!

credentials
	^credentials!

credentials: anObject
	credentials := anObject!

dolphinOwner

	^self ownerNamed: GitHubOwner dolphinOwnerName!

hasCredentials

	^self credentials 
		ifNil: [false] 
		ifNotNil: [ :creds | creds isComplete]!

initialize

	self 
		repositories: CaseInsensitiveSearchPolicy current newLookupTable;
		owners: CaseInsensitiveSearchPolicy current newLookupTable;
		credentials: GitHubCredentials default!

mediaType

	^##(self) mediaType!

newOwner

	^GitHubOwner new
		api: self;
		yourself!

newRepository

	^GitHubRepository new
		api: self;
		yourself!

newRequest

	^self newRequestWithURL: self url!

newRequestWithURL: aString

	| request |

	request := RESTRequest url: aString.

	request
		addRequestHeader: 'Accept' value: self mediaType;
		addParameter: 'per_page' value: 100. "avoid paging for now (hopefully) by requesting max per page"

	self hasCredentials ifTrue: [self credentials addRequestHeadersTo: request].

	request when: #responseReceived: send: #onResponseReceived: to: self.

	^request!

onResponseReceived: aRESTResponse

	aRESTResponse isSuccessful ifTrue: 
		[(aRESTResponse responseHeader: 'X-RateLimit-Limit') ifNotNil: [ :limitString | self rateLimit: (Integer readFrom: limitString readStream)].
		(aRESTResponse responseHeader: 'X-RateLimit-Remaining') ifNotNil: [ :remainingString | self rateLimitRemaining: (Integer readFrom: remainingString readStream)]]!

organisationRepositoriesRequestFor: aString

	^(self newRequest, {'orgs'. aString. 'repos'}) get; yourself!

ownerFromJson: aDictionary

	^self owners at: (aDictionary at: 'login') ifAbsentPut: [self newOwner initializeFromJson: aDictionary]!

ownerNamed: aString

	^self ownerNamed: aString ifNone: [nil]!

ownerNamed: aString ifNone: aBlock

	"Don't return invalid owners here"

	^(self _ownerNamed: aString) ifNotValid: aBlock!

owners
	^owners!

owners: anObject
	owners := anObject!

rateLimit
	^rateLimit!

rateLimit: anObject
	rateLimit := anObject!

rateLimitRemaining
	^rateLimitRemaining!

rateLimitRemaining: anObject
	rateLimitRemaining := anObject!

refresh

	self owners do: [ :each | each refresh].
	self repositories do: [ :each | each refresh]!

removeOwner: anOwner

	^self owners removeKey: anOwner name!

removeRepository: aRepository

	| owner |

	owner := aRepository _owner.

	self repositories removeKey: aRepository gitHubPath.

	owner isNil ifFalse: 
		[owner removeRepository: aRepository.
		owner repositories isEmpty ifTrue: [self removeOwner: owner]].

	^aRepository
	!

repositories
	^repositories!

repositories: anObject
	repositories := anObject!

repositoryFromJson: aDictionary

	| ownerName repositoryName | 

	ownerName := (aDictionary at: 'owner') at: 'login'.
	repositoryName := aDictionary at: 'name'.

	^self repositoryWithPath: (ownerName, '/', repositoryName) ifAbsentPut: [self newRepository initializeFromJson: aDictionary]!

repositoryWithPath: aString

	"user/name e.g. dolphinsmalltalk/dolphin"

	^self repositoryWithPath: aString ifAbsentPut:
		[| parts |
		parts := aString subStrings: $/.
		parts size = 2 ifFalse: [self error: 'invalid path'].
		GitHubRepository new
			name: parts second;
			ownerName: parts first;
			api: self; 
			yourself]!

repositoryWithPath: aString ifAbsentPut: aBlock

	"Private - user/name e.g. dolphinsmalltalk/dolphin"

	^self repositories at: aString ifAbsentPut: aBlock!

url

	^##(self) url!

userRepositoriesRequestFor: aString

	^self newRequest, {'users'. aString. 'repos'}! !
!GitHubAPI categoriesFor: #_ownerNamed:!accessing!private! !
!GitHubAPI categoriesFor: #canAuthenticate!public!testing! !
!GitHubAPI categoriesFor: #credentials!accessing!public! !
!GitHubAPI categoriesFor: #credentials:!accessing!public! !
!GitHubAPI categoriesFor: #dolphinOwner!accessing!public! !
!GitHubAPI categoriesFor: #hasCredentials!public!testing! !
!GitHubAPI categoriesFor: #initialize!initialize/release!public! !
!GitHubAPI categoriesFor: #mediaType!constants!public! !
!GitHubAPI categoriesFor: #newOwner!accessing!private! !
!GitHubAPI categoriesFor: #newRepository!accessing!private! !
!GitHubAPI categoriesFor: #newRequest!helpers!public! !
!GitHubAPI categoriesFor: #newRequestWithURL:!helpers!public! !
!GitHubAPI categoriesFor: #onResponseReceived:!event handling!public! !
!GitHubAPI categoriesFor: #organisationRepositoriesRequestFor:!accessing!private! !
!GitHubAPI categoriesFor: #ownerFromJson:!accessing!private! !
!GitHubAPI categoriesFor: #ownerNamed:!accessing!public! !
!GitHubAPI categoriesFor: #ownerNamed:ifNone:!accessing!public! !
!GitHubAPI categoriesFor: #owners!accessing!private! !
!GitHubAPI categoriesFor: #owners:!accessing!private! !
!GitHubAPI categoriesFor: #rateLimit!accessing!public! !
!GitHubAPI categoriesFor: #rateLimit:!accessing!public! !
!GitHubAPI categoriesFor: #rateLimitRemaining!accessing!public! !
!GitHubAPI categoriesFor: #rateLimitRemaining:!accessing!public! !
!GitHubAPI categoriesFor: #refresh!initialize/release!public! !
!GitHubAPI categoriesFor: #removeOwner:!public!removing! !
!GitHubAPI categoriesFor: #removeRepository:!public!removing! !
!GitHubAPI categoriesFor: #repositories!accessing!private! !
!GitHubAPI categoriesFor: #repositories:!accessing!private! !
!GitHubAPI categoriesFor: #repositoryFromJson:!accessing!private! !
!GitHubAPI categoriesFor: #repositoryWithPath:!accessing!public! !
!GitHubAPI categoriesFor: #repositoryWithPath:ifAbsentPut:!accessing!private! !
!GitHubAPI categoriesFor: #url!constants!public! !
!GitHubAPI categoriesFor: #userRepositoriesRequestFor:!accessing!private! !

!GitHubAPI class methodsFor!

mediaType

	^'application/vnd.github.v3.raw+json'!

new

	^super new initialize!

url

	^'https://api.github.com'! !
!GitHubAPI class categoriesFor: #mediaType!constants!public! !
!GitHubAPI class categoriesFor: #new!instance creation!public! !
!GitHubAPI class categoriesFor: #url!constants!public! !

