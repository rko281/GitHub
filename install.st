| tempFilename shell zipFile zipRepositoryFolder zipRepositoryItems destination |
tempFilename := URLMonLibrary default urlDownloadToCacheFile: 'https://api.github.com/repos/rko281/GitHub/zipball/master'.
shell := IDispatch createObject: 'Shell.Application'.
zipFile := shell nameSpace: tempFilename.
zipRepositoryFolder := shell nameSpace: (File composePath: tempFilename subPath: (zipFile items item: 0) name).
zipRepositoryItems := zipRepositoryFolder items.
zipRepositoryItems filter: 16r20 pattern: 'GitHub'.
zipRepositoryFolder := (zipRepositoryItems item: 0) getFolder.
zipRepositoryItems := zipRepositoryFolder items.
destination := File fullPathOf: 'GitHub' relativeTo: SessionManager current imageBase.
File createDirectoryPath: destination.
(shell nameSpace: destination) copyHere: zipRepositoryItems vOptions: 4.
File delete: tempFilename.
Package manager install: (File composePath: destination subPath: 'GitHub Packages')!
