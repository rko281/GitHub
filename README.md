# GitHub for Dolphin Smalltalk 7.1
* Download and install Dolphin Smalltalk Packages and their prerequisites directly from GitHub
* Simplifies the loading of complex frameworks
* Uses the GitHub API v3
* Developed and tested with Dolphin Smalltalk 7.1

## Getting Started
* Download the repository to a subdirectory of your Dolphin working directory (e.g. `GitHub`)
* Install the package `GitHub Packages` (this will load all required prerequisite packages)
* To download and install a package from GitHub evaluate:

  `GitHubPackageManager install: '<github username>/<repository name>/<package path>'`
    
  Example:
  
  `GitHubPackageManager install: 'rko281/MethodSpies/Method Spies'`
  
  Package name can be omitted where it matches the repository name:
  
  `GitHubPackageManager install: 'rko281/MethodSpies'`

* Any required prerequisite packages will also be downloaded (this includes prerequisites from other repositories where these can be located)

## Security
* The first time you install a package you'll be prompted for your GitHub username and password - this is used to request a [personal access token](https://blog.github.com/2013-05-16-personal-api-tokens/) to authenticate subsequent GitHub API requests. 
* **Your username and the token are stored in the image**
* **Optionally they may also be stored in the registry for use by other images**
* **Please ensure you understand the security implications of storing this information before using this package**

## Acknowledgement
For simplicity of installation this repository includes the US JSON package from the [Dolphin Smalltalk Contributions](https://github.com/dolphinsmalltalk/Contributions) repository
