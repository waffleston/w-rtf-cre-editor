## Contributing
* [Pull Requests](#pull-requests)
  * Branch Naming Convention
  * Environment
  * Style Guide
* [Bug Reports](#bug-reports)
  * Check for Similar Issues
  * Create an Issue

Welcome, and thank you for considering contributing to this project!

Question, suggestion, or something that isn't really a bug report or pull request?  
Feel free to contact me at waffles@nawibo.com.
### Pull Requests
#### Branch Naming Convention
Bug fix: "#-"  
Prefix with the issue number.  
If there's no issue, create it and mention that you're going to pull request.  
ex: `1-crashes-on-ctrl-v`

Adding a listed feature: "listed-"  
ex: `listed-respect-header`

Adding a new, never before suggested feature: "suggest-"  
ex: `suggest-rewrite-color-lib`
#### Environment
All binaries are compiled with gfortran, so ensuring your changes work in that compiler would be appreciated.
#### Style Guide
Thus far, there has been a loose set of guidelines in place.
* 8 spaces for indentation.  (Vim automatically converts tabs to 8 spaces.)
* Variables should be declared first, and then given a value later.  Do not assign a value at definition unless it is a global.
* Keywords, scoped variables lowercase, globals mixedCase, externals underscore_lowercase.
* Spaces after commas unless inside (), [], {}.
* Comments delineated by ` ` `!` ` `, or `!` ` ` after a tab.
* Use full notation for defining variables, ex: `integer :: `.
* Assume `implicit none`.
* Ensure that functionality NOT specific to the terminal remains in the universal module and does not print or write to the screen.
* Ensure that functionality specific to the terminal remains in the terminal module.
* Only commit after you've tested the code and don't see any possible issues.  A feature should be added in a single commit unless it's really that complicated.
### Bug Reports
#### Check For Similar Issues
Be sure to search the existing and closed issues for your problem, and contribute to the discussion when applicable.
#### Create an Issue
Title:  
Short description of the problem  
ex: `Crashes on ctrl+v.`

Prefix the message with a block block containing your operating system and the version from running `wrtfe -v`.  
ex:
```
Windows 7 x64
w-rtf-cre-editor v0.0.54 (2017-jul-13)
```

If you plan on submitting a pull request related to this issue, mention that at the end.
