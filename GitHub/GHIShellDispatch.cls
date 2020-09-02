"Filed out from Dolphin Smalltalk 7"!

IDispatch subclass: #GHIShellDispatch
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'GHShell32Constants'
	classInstanceVariableNames: ''!
GHIShellDispatch guid: (IID fromString: '{D8F015C0-C278-11CE-A49E-444553540000}')!
GHIShellDispatch comment: '`GHShell32IShellDispatch` is a wrapper class for the COM interface ''Shell32.IShellDispatch'' generated from type information in the ''Microsoft Shell Controls And Automation'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Definition of interface IShellDispatch"

** This comment was automatically generated from a type library. Delete this line to prevent any manual edits from being overwritten if the wrapper class is regenerated.

IDL definition follows:
```
[
	object, 
	uuid(d8f015c0-c278-11ce-a49e-444553540000), 
	helpstring("Definition of interface IShellDispatch"), 
	hidden, 
	dual
]
interface IShellDispatch : IDispatch
 {
	[id(0x60020000), propget, helpstring("Get Application object")]
	HRESULT __stdcall Application(
		[out, retval]IDispatch** ppid);
	[id(0x60020001), propget, helpstring("Get Parent object")]
	HRESULT __stdcall Parent(
		[out, retval]IDispatch** ppid);
	[id(0x60020002), helpstring("Get special folder from ShellSpecialFolderConstants")]
	HRESULT __stdcall NameSpace(
		[in]VARIANT vDir,
		[out, retval]Folder** ppsdf);
	[id(0x60020003), helpstring("Browse the name space for a Folder")]
	HRESULT __stdcall BrowseForFolder(
		[in]long Hwnd,
		[in]BSTR Title,
		[in]long Options,
		[in, optional]VARIANT RootFolder,
		[out, retval]Folder** ppsdf);
	[id(0x60020004), helpstring("The collection of open folder windows")]
	HRESULT __stdcall Windows(
		[out, retval]IDispatch** ppid);
	[id(0x60020005), helpstring("Open a folder")]
	HRESULT __stdcall Open(
		[in]VARIANT vDir);
	[id(0x60020006), helpstring("Explore a folder")]
	HRESULT __stdcall Explore(
		[in]VARIANT vDir);
	[id(0x60020007), helpstring("Minimize all windows")]
	HRESULT __stdcall MinimizeAll();
	[id(0x60020008), helpstring("Undo Minimize All")]
	HRESULT __stdcall UndoMinimizeALL();
	[id(0x60020009), helpstring("Bring up the file run")]
	HRESULT __stdcall FileRun();
	[id(0x6002000a), helpstring("Cascade Windows")]
	HRESULT __stdcall CascadeWindows();
	[id(0x6002000b), helpstring("Tile windows vertically")]
	HRESULT __stdcall TileVertically();
	[id(0x6002000c), helpstring("Tile windows horizontally")]
	HRESULT __stdcall TileHorizontally();
	[id(0x6002000d), helpstring("Exit Windows")]
	HRESULT __stdcall ShutdownWindows();
	[id(0x6002000e), helpstring("Suspend the pc")]
	HRESULT __stdcall Suspend();
	[id(0x6002000f), helpstring("Eject the pc")]
	HRESULT __stdcall EjectPC();
	[id(0x60020010), helpstring("Bring up the Set time dialog")]
	HRESULT __stdcall SetTime();
	[id(0x60020011), helpstring("Handle Tray properties")]
	HRESULT __stdcall TrayProperties();
	[id(0x60020012), helpstring("Display shell help")]
	HRESULT __stdcall Help();
	[id(0x60020013), helpstring("Find Files")]
	HRESULT __stdcall FindFiles();
	[id(0x60020014), helpstring("Find a computer")]
	HRESULT __stdcall FindComputer();
	[id(0x60020015), helpstring("Refresh the menu")]
	HRESULT __stdcall RefreshMenu();
	[id(0x60020016), helpstring("Run a Control Panel Item")]
	HRESULT __stdcall ControlPanelItem(
		[in]BSTR bstrDir);
};
```
'!
!GHIShellDispatch categoriesForClass!COM-Interfaces!Shell32-Interfaces! !
!GHIShellDispatch methodsFor!

nameSpace: vDir
	"Answer the <GHShell32Folder3> result of invoking the NameSpace() method of the COM object.
	Helpstring: Get special folder from ShellSpecialFolderConstants"

	| answer |
	answer := GHShell32Folder3 newPointer.
	self NameSpace: vDir asVariant ppsdf: answer.
	^answer asObject!

NameSpace: vDir ppsdf: ppsdf
	"Private - Invoke the NameSpace() method of the COM object.
	Helpstring: Get special folder from ShellSpecialFolderConstants

		HRESULT __stdcall NameSpace(
			[in]VARIANT vDir,
			[out, retval]Folder** ppsdf);"

	<virtual stdcall: hresult 10 variant GHShell32Folder3**>
	^self invalidCall: _failureCode! !
!GHIShellDispatch categoriesFor: #nameSpace:!**auto generated**!methods!public! !
!GHIShellDispatch categoriesFor: #NameSpace:ppsdf:!**auto generated**!COM Interfaces-IShellDispatch!private! !

!GHIShellDispatch class methodsFor!

clsid
	"Private - Answer the CLSID of the coclass (Shell) for which the receiver is the default interface."

	^CLSID fromString: '{13709620-c279-11ce-a49e-444553540000}'
!

defineFunctions
	"Declare the virtual function table for the COM interface 'Shell32.IShellDispatch'
		GHShell32IShellDispatch defineTemplate"

	self
		defineFunction: #NameSpace:ppsdf:
			argumentTypes: 'variant GHShell32Folder3**'!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		GHShell32IShellDispatch initializeTypeLib"

	typeLib := GHShell32Lib! !
!GHIShellDispatch class categoriesFor: #clsid!**auto generated**!constants!private! !
!GHIShellDispatch class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!GHIShellDispatch class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

