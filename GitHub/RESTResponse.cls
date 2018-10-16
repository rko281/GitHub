"Filed out from Dolphin Smalltalk 7"!

Object subclass: #RESTResponse
	instanceVariableNames: 'status statusText responseText responseHeadersText responseHeaders'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RESTResponse guid: (GUID fromString: '{4966c7c1-5f3a-4732-ae01-e693f32aaddd}')!
RESTResponse comment: ''!
!RESTResponse categoriesForClass!Kernel-Objects! !
!RESTResponse methodsFor!

charset

	| stream |

	stream := self contentType readStream.
	^(stream skipToAll: 'charset=')
		ifTrue: [stream upTo: $;]
		ifFalse: ['']!

contentType

	^self responseHeader: 'content-type'!

eTag

	^self responseHeaders at: 'ETag' ifAbsent: [nil]!

isNotModified

	^self status = 304!

isRedirected

	^self status between: 300 and: 399!

isSuccessful

	^self status between: 200 and: 299!

isUTF8

	^'utf-8' match: self charset!

json

	"ASSERT: the responseText is valid JSON"

	^Object fromJsonString: self responseText!

readResponseFrom: aWinHttpRequest

	self readResponseStatusFrom: aWinHttpRequest.
	self isNotModified ifTrue: [^self].

	self 
		readResponseHeadersFrom: aWinHttpRequest;
		readResponseTextFrom: aWinHttpRequest!

readResponseHeadersFrom: aWinHttpRequest

	| stream |

	self responseHeadersText: aWinHttpRequest getAllResponseHeaders.
	self responseHeaders: CaseInsensitiveSearchPolicy current newLookupTable.

	stream := self responseHeadersText readStream.

	[stream skipSeparators.
	stream atEnd] whileFalse: 
		[| key value |
		key := stream upTo: $:.
		stream skipSeparators.
		value := stream nextLine.
		self responseHeaders at: key put: value]!

readResponseStatusFrom: aHttpRequest

	self 
		status: aHttpRequest status;
		statusText: aHttpRequest statusText!

readResponseTextFrom: aHttpRequest

	"Non-UTF8 content appears to be sometimes corrupted using responseText"
	self isUTF8
		ifTrue: [self responseText: aHttpRequest responseText]
		ifFalse: [self responseText:  aHttpRequest responseBody asObject asString]!

responseHeader: aString

	^self responseHeaders at: aString ifAbsent: [nil]!

responseHeaders
	^responseHeaders!

responseHeaders: anObject
	responseHeaders := anObject!

responseHeadersText
	^responseHeadersText!

responseHeadersText: anObject
	responseHeadersText := anObject!

responseText
	^responseText!

responseText: anObject
	responseText := anObject!

status
	^status!

status: anObject
	status := anObject!

statusText
	^statusText!

statusText: anObject
	statusText := anObject! !
!RESTResponse categoriesFor: #charset!accessing!public! !
!RESTResponse categoriesFor: #contentType!accessing!public! !
!RESTResponse categoriesFor: #eTag!accessing!public! !
!RESTResponse categoriesFor: #isNotModified!public!testing! !
!RESTResponse categoriesFor: #isRedirected!public!testing! !
!RESTResponse categoriesFor: #isSuccessful!public!testing! !
!RESTResponse categoriesFor: #isUTF8!public!testing! !
!RESTResponse categoriesFor: #json!accessing!public! !
!RESTResponse categoriesFor: #readResponseFrom:!executing!public! !
!RESTResponse categoriesFor: #readResponseHeadersFrom:!executing!private! !
!RESTResponse categoriesFor: #readResponseStatusFrom:!executing!private! !
!RESTResponse categoriesFor: #readResponseTextFrom:!executing!private! !
!RESTResponse categoriesFor: #responseHeader:!accessing!public! !
!RESTResponse categoriesFor: #responseHeaders!accessing!public! !
!RESTResponse categoriesFor: #responseHeaders:!accessing!private! !
!RESTResponse categoriesFor: #responseHeadersText!accessing!public! !
!RESTResponse categoriesFor: #responseHeadersText:!accessing!private! !
!RESTResponse categoriesFor: #responseText!accessing!public! !
!RESTResponse categoriesFor: #responseText:!accessing!private! !
!RESTResponse categoriesFor: #status!accessing!public! !
!RESTResponse categoriesFor: #status:!accessing!private! !
!RESTResponse categoriesFor: #statusText!accessing!public! !
!RESTResponse categoriesFor: #statusText:!accessing!private! !

!RESTResponse class methodsFor!

from: aWinHttpRequest

	^self new
		readResponseFrom: aWinHttpRequest;
		yourself!

fromWin32Error: aWin32Error

	"Create a fake response to wrap the information in aWin32Error"

	^self new
		status: 404;
		statusText: aWin32Error description;
		responseText: '';
		responseHeadersText: '';
		yourself! !
!RESTResponse class categoriesFor: #from:!instance creation!public! !
!RESTResponse class categoriesFor: #fromWin32Error:!instance creation!public! !

