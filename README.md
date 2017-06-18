# w-rtf-cre-editor
A stenography RTF/CRE dictionary editor. 
## What is it?
It's a lightweight RTF/CRE dictionary editor, aiming to be as easy to use as possible without sacrificing advanced functionality.  There are a number of useful features, including:
* Duplicate entry detection & resolution
* Entry validation
* Dictionary searches (both strokes and translations)
## [Download](build/)
(Windows and GNU/Linux)  
It's currently a terminal-only program, but I'm working on an Electron based wrapper for it.
## Todo:
* [x] Dictionary Editing (Cont.)
  * [ ] Global/multi replace
* [ ] Metadata
  * [x] Header
    * [x] Maintain it.
    * [x] Replace system text.
    * [ ] Read modified keyboard arrangement?
    * [ ] Dates
* [x] Create backup dictionary.
  * [x] `to` command.
* [ ] Create alternate dictionaries targeting different standards of compliance.
* [ ] TUI?
  * [ ] Colors in terminal
* [ ] GUI
  * [ ] Electron
