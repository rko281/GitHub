"Filed out from Dolphin Smalltalk 7"!

GitHubInfoObject subclass: #GitHubOwner
	instanceVariableNames: 'repositories'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubOwner guid: (GUID fromString: '{61bde4f8-6aba-45bc-9df4-e9e1009356f1}')!
GitHubOwner comment: ''!
!GitHubOwner categoriesForClass!Kernel-Objects! !
!GitHubOwner methodsFor!

= aGitHubUser

	^self name = aGitHubUser name!

addRepository: aRepository

	^self repositories at: aRepository name put: aRepository!

addRepositoryNamed: aString

	^self addRepository: (self api repositoryWithPath: (self name, '/', aString))!

allRepositories

	| request |

	"We use a user repos request since this appears to work for organisations as well as users. The reverse is not true"
	request := self api userRepositoriesRequestFor: self name.
	request get.

	^request json collect: [ :each | self api repositoryFromJson: each]!

flushLocalCaches

	self initializeRepositories!

hash

	^self name hash!

initialize

	self initializeRepositories!

initializeFromJson: aDictionary

	super initializeFromJson: aDictionary.

	self name: (aDictionary at: 'login')!

initializeRepositories

	self repositories: CaseInsensitiveSearchPolicy current newLookupTable!

isDolphinOwner

	^self name = ##(self) dolphinOwnerName!

isOrganization

	^self type = 'Organization'!

isUser

	^self type = 'User'!

newRequest

	"We use a user repos request since this appears to work for organisations as well as users. The reverse is not true"
	^super newRequest, {'users'. self name}!

removeRepository: aRepository

	^self repositories removeKey: aRepository name!

repositories

	^repositories!

repositories: aLookupTable

	repositories := aLookupTable!

repositoryNamed: aString

	^self repositoryNamed: aString ifNone: [nil]!

repositoryNamed: aString ifNone: aBlock

	"Don't answer invalid repositoroies here"

	^(self repositories at: aString ifAbsent: [self addRepositoryNamed: aString]) ifNotValid: aBlock!

type

	^self json at: 'type'! !
!GitHubOwner categoriesFor: #=!comparing!public! !
!GitHubOwner categoriesFor: #addRepository:!adding!public! !
!GitHubOwner categoriesFor: #addRepositoryNamed:!adding!public! !
!GitHubOwner categoriesFor: #allRepositories!accessing!public! !
!GitHubOwner categoriesFor: #flushLocalCaches!initialize/release!private! !
!GitHubOwner categoriesFor: #hash!comparing!public! !
!GitHubOwner categoriesFor: #initialize!initialize/release!public! !
!GitHubOwner categoriesFor: #initializeFromJson:!initialize/release!private! !
!GitHubOwner categoriesFor: #initializeRepositories!initialize/release!private! !
!GitHubOwner categoriesFor: #isDolphinOwner!public!testing! !
!GitHubOwner categoriesFor: #isOrganization!public!testing! !
!GitHubOwner categoriesFor: #isUser!public!testing! !
!GitHubOwner categoriesFor: #newRequest!accessing!private! !
!GitHubOwner categoriesFor: #removeRepository:!public!removing! !
!GitHubOwner categoriesFor: #repositories!accessing!public! !
!GitHubOwner categoriesFor: #repositories:!accessing!public! !
!GitHubOwner categoriesFor: #repositoryNamed:!accessing!public! !
!GitHubOwner categoriesFor: #repositoryNamed:ifNone:!accessing!public! !
!GitHubOwner categoriesFor: #type!accessing!public! !

!GitHubOwner class methodsFor!

dolphinOwnerName

	^'dolphinsmalltalk'!

new

	^super new initialize! !
!GitHubOwner class categoriesFor: #dolphinOwnerName!constants!public! !
!GitHubOwner class categoriesFor: #new!public! !

