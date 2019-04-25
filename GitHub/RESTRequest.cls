"Filed out from Dolphin Smalltalk 7"!

Object subclass: #RESTRequest
	instanceVariableNames: 'url parameters content requestHeaders isModified lastResponse events'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RESTRequest guid: (GUID fromString: '{15168f22-aded-48a0-a2a2-2dea4e309efd}')!
RESTRequest comment: ''!
!RESTRequest categoriesForClass!Unclassified! !
!RESTRequest methodsFor!

, aCollection

	| stream |

	stream := String writeStream: (self url size * 2).

	stream nextPutAll: self url.
	aCollection do: [ :each | stream nextPut: $/; nextPutAll: each].

	self url: stream contents!

addParameter: aString value: anObject

	self parameters addLast: (aString -> anObject)!

addRequestHeader: aString value: anObject

	self requestHeaders at: aString put: anObject!

content
	^content!

content: anObject
	content := anObject!

delete

	^self executeWithVerb: 'DELETE' includeContent: true!

executeWithVerb: aString includeContent: aBoolean

	| httpRequest |

	httpRequest := self httpRequestFor: aString.

	[aBoolean 
		ifTrue: [httpRequest send: self content]
		ifFalse: [httpRequest send].

	self trigger: #executedWithVerb: with: aString.
	self readResponseFrom: httpRequest] on: Win32Error do: [ :exc | self lastResponse: (RESTResponse fromWin32Error: exc). exc return].

	^self lastResponse!

get

	^self executeWithVerb: 'GET' includeContent: false!

getEvents
	"Private - Answer the EventsCollection belonging to the receiver, or nil if the receiver
	has no events registered for it"

	^events!

hasParameters 

	^self parameters isEmpty not!

httpRequestFor: aString

	"Private - aString is the HTTP verb (GET, POST etc.)"

	| httpRequest |

	httpRequest := IWinHttpRequest new.

	httpRequest open: aString url: self urlWithParameters.
	self setRequestHeadersIn: httpRequest.

	self isExecuted ifTrue: [self lastResponse eTag ifNotNil: [ :eTag | httpRequest setRequestHeader: 'If-None-Match' value: eTag]].

	^httpRequest!

isExecuted

	^self lastResponse notNil!

isModified

	^isModified ifNil: [false]!

isModified: aBoolean
	isModified := aBoolean!

isSuccessful

	self verifyExecuted.

	^self lastResponse isSuccessful!

json

	self verifyExecuted.

	^self lastResponse json!

lastResponse
	^lastResponse!

lastResponse: anObject
	lastResponse := anObject!

parameters
	^parameters!

parameters: anObject
	parameters := anObject!

patch

	^self executeWithVerb: 'PATCH' includeContent: true!

post

	^self executeWithVerb: 'POST' includeContent: true!

put

	^self executeWithVerb: 'PUT' includeContent: true!

readResponseFrom: aHttpRequest

	| newResponse |

	newResponse := RESTResponse from: aHttpRequest.
	self trigger: #responseReceived: with: newResponse.
	newResponse isNotModified 
		ifTrue: [self isModified: false]
		ifFalse: [self isModified: true; lastResponse: newResponse]!

requestHeaders
	^requestHeaders!

requestHeaders: anObject
	requestHeaders := anObject!

response

	self verifyExecuted.

	^self lastResponse!

responseText

	^self response responseText!

setEvents: anEventsCollectionOrNil 
	"Private - Set the EventsCollection of the receiver to be anEventsCollectionOrNil.
	Answer the receiver."

	events := anEventsCollectionOrNil!

setRequestHeadersIn: aWinHttpRequest

	self requestHeaders keysAndValuesDo: [ :header :value | aWinHttpRequest setRequestHeader: header value: value displayString]!

url
	^url!

url: anObject
	url := anObject!

urlWithParameters

	| stream |

	stream := String writeStream: 64.

	stream nextPutAll: self url.

	self hasParameters ifTrue: 
		[stream nextPut: $?.
		self parameters do: [ :assoc | stream nextPutAll: assoc key; nextPut: $=; display: assoc value] separatedBy: [stream nextPut: $&]].

	^stream contents!

verifyExecuted

	self isExecuted ifFalse: [self error: 'not executed']! !
!RESTRequest categoriesFor: #,!adding!public! !
!RESTRequest categoriesFor: #addParameter:value:!adding!public! !
!RESTRequest categoriesFor: #addRequestHeader:value:!adding!public! !
!RESTRequest categoriesFor: #content!accessing!public! !
!RESTRequest categoriesFor: #content:!accessing!public! !
!RESTRequest categoriesFor: #delete!executing!public! !
!RESTRequest categoriesFor: #executeWithVerb:includeContent:!executing!private! !
!RESTRequest categoriesFor: #get!executing!public! !
!RESTRequest categoriesFor: #getEvents!accessing!private! !
!RESTRequest categoriesFor: #hasParameters!public!testing! !
!RESTRequest categoriesFor: #httpRequestFor:!executing!private! !
!RESTRequest categoriesFor: #isExecuted!public!testing! !
!RESTRequest categoriesFor: #isModified!public!testing! !
!RESTRequest categoriesFor: #isModified:!accessing!private! !
!RESTRequest categoriesFor: #isSuccessful!public!testing! !
!RESTRequest categoriesFor: #json!accessing!public! !
!RESTRequest categoriesFor: #lastResponse!accessing!private! !
!RESTRequest categoriesFor: #lastResponse:!accessing!private! !
!RESTRequest categoriesFor: #parameters!accessing!public! !
!RESTRequest categoriesFor: #parameters:!accessing!public! !
!RESTRequest categoriesFor: #patch!executing!public! !
!RESTRequest categoriesFor: #post!executing!public! !
!RESTRequest categoriesFor: #put!executing!public! !
!RESTRequest categoriesFor: #readResponseFrom:!executing!private! !
!RESTRequest categoriesFor: #requestHeaders!accessing!public! !
!RESTRequest categoriesFor: #requestHeaders:!accessing!public! !
!RESTRequest categoriesFor: #response!accessing!public! !
!RESTRequest categoriesFor: #responseText!accessing!public! !
!RESTRequest categoriesFor: #setEvents:!accessing!private! !
!RESTRequest categoriesFor: #setRequestHeadersIn:!executing!private! !
!RESTRequest categoriesFor: #url!accessing!public! !
!RESTRequest categoriesFor: #url:!accessing!public! !
!RESTRequest categoriesFor: #urlWithParameters!accessing!public! !
!RESTRequest categoriesFor: #verifyExecuted!executing!private! !

!RESTRequest class methodsFor!

url: aString

	^self url: aString parameters: OrderedCollection new!

url: aString parameters: paramsCollection

	^self url: aString parameters: paramsCollection requestHeaders: Dictionary new!

url: aString parameters: paramsCollection requestHeaders: aDictionary

	^self new
		url: aString;
		parameters: paramsCollection;
		requestHeaders: aDictionary;
		yourself! !
!RESTRequest class categoriesFor: #url:!public! !
!RESTRequest class categoriesFor: #url:parameters:!public! !
!RESTRequest class categoriesFor: #url:parameters:requestHeaders:!public! !

