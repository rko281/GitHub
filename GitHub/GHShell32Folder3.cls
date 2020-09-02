"Filed out from Dolphin Smalltalk 7"!

IDispatch subclass: #GHShell32Folder3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GHShell32Folder3 guid: (IID fromString: '{A7AE5F64-C4D7-4D7F-9307-4D24EE54B841}')!
GHShell32Folder3 comment: '`GHFolder3` is a wrapper class for the COM interface ''Shell32.Folder3'' generated from type information in the ''Microsoft Shell Controls And Automation'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Definition of interface Folder version 3"

** This comment was automatically generated from a type library. Delete this line to prevent any manual edits from being overwritten if the wrapper class is regenerated.

IDL definition follows:
```
[
	object, 
	uuid(a7ae5f64-c4d7-4d7f-9307-4d24ee54b841), 
	helpstring("Definition of interface Folder version 3"), 
	dual
]
interface Folder3 : Folder2
 {
	[id(0x00000002), propget, helpstring("Ask if the WebView barricade should be shown or not")]
	HRESULT __stdcall ShowWebViewBarricade(
		[out, retval]VARIANT_BOOL* pbShowWebViewBarricade);
	[id(0x00000002), propput, helpstring("Ask if the WebView barricade should be shown or not")]
	HRESULT __stdcall ShowWebViewBarricade(
		[in]VARIANT_BOOL pbShowWebViewBarricade);
};
```
'!
!GHShell32Folder3 categoriesForClass!COM-Interfaces!Shell32-Interfaces! !
!GHShell32Folder3 methodsFor!

copyHere: vItem vOptions: vOptions
	"Invoke the CopyHere() method of the COM object.
	Helpstring: Copy Items to this folder."

	^self CopyHere: vItem asVariant vOptions: vOptions asVariant!

CopyHere: vItem vOptions: vOptions
	"Private - Invoke the CopyHere() method of the COM object.
	Helpstring: Copy Items to this folder.

		HRESULT __stdcall CopyHere(
			[in]VARIANT vItem,
			[in, optional]VARIANT vOptions);"

	<virtual stdcall: hresult 16 variant variant>
	^self invalidCall: _failureCode!

get_Title: pbs
	"Private - Get the value of the 'Title' property of the receiver.

		HRESULT __stdcall Title(
			[out, retval]BSTR* pbs);"

	<virtual stdcall: hresult 8 bstr*>
	^self invalidCall: _failureCode!

items
	"Answer the <GHShell32FolderItems> result of invoking the Items() method of the COM object.
	Helpstring: The collection of Items in folder"

	| answer |
	answer := GHShell32FolderItems newPointer.
	self Items: answer.
	^answer asObject!

Items: ppid
	"Private - Invoke the Items() method of the COM object.
	Helpstring: The collection of Items in folder

		HRESULT __stdcall Items(
			[out, retval]FolderItems** ppid);"

	<virtual stdcall: hresult 12 GHShell32FolderItems**>
	^self invalidCall: _failureCode!

parseName: bName
	"Answer the <Shell32FolderItem> result of invoking the ParseName() method of the COM object.
	Helpstring: Parse the name to get an item."

	| answer |
	answer := GHShell32FolderItem newPointer.
	self ParseName: bName ppid: answer.
	^answer asObject!

ParseName: bName ppid: ppid
	"Private - Invoke the ParseName() method of the COM object.
	Helpstring: Parse the name to get an item.

		HRESULT __stdcall ParseName(
			[in]BSTR bName,
			[out, retval]FolderItem** ppid);"

	<virtual stdcall: hresult 13 bstr GHShell32FolderItem**>
	^self invalidCall: _failureCode!

title
	"Answer the <bstr> value of the 'Title' property of the receiver.
	Helpstring: Get the display name for the window"

	| answer |
	answer := BSTR new.
	self get_Title: answer.
	^answer asObject!

value
	"Answer the <bstr> value of the 'Title' property of the receiver.
	This is the default value (DISPID_VALUE) property of the receiver."

	^self title! !
!GHShell32Folder3 categoriesFor: #copyHere:vOptions:!**auto generated**!methods!public! !
!GHShell32Folder3 categoriesFor: #CopyHere:vOptions:!**auto generated**!COM Interfaces-Folder!private! !
!GHShell32Folder3 categoriesFor: #get_Title:!**auto generated**!COM Interfaces-Folder!private! !
!GHShell32Folder3 categoriesFor: #items!**auto generated**!methods!public! !
!GHShell32Folder3 categoriesFor: #Items:!**auto generated**!COM Interfaces-Folder!private! !
!GHShell32Folder3 categoriesFor: #parseName:!methods!public! !
!GHShell32Folder3 categoriesFor: #ParseName:ppid:!**auto generated**!COM Interfaces-Folder!private! !
!GHShell32Folder3 categoriesFor: #title!**auto generated**!properties!public! !
!GHShell32Folder3 categoriesFor: #value!**auto generated**!properties!public! !

!GHShell32Folder3 class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'Shell32.Folder'
		GHShell32Folder3 defineTemplate"

	self
		defineFunction: #get_Title:
			argumentTypes: 'bstr*';
		defineFunction: #Items:
			argumentTypes: 'GHShell32FolderItems**';
		defineFunction: #ParseName:ppid:
			argumentTypes: 'bstr GHShell32FolderItem**';
		defineFunction: #CopyHere:vOptions:
			argumentTypes: 'variant variant'!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		GHShell32Folder3 initializeTypeLib"

	typeLib := GHShell32Lib! !
!GHShell32Folder3 class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!GHShell32Folder3 class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

