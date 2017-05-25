# w-rtf-cre-editor
A stenography RTF/CRE dictionary editor. 
## What is it?
It's a lightweight RTF/CRE dictionary editor, aiming to be as easy to use as possible without sacrificing advanced functionality.  There are a number of useful features, including:
* Duplicate entry detection & resolution
* Entry validation
* Dictionary searches (both steno and translations)
## [Download](build/)
(Windows and GNU/Linux)  
It's currently a terminal-only program, but I'm taking steps to make it easy to integrate with a GUI.
## Todo:
* [x] Terminal
  * [x] REPL dictionary editing.
    * [ ] Color REPL?
  * [X] CLI arguments to edit dictionary without REPL.
* [x] Dictionary Editing
  * [x] Search
  * [x] Add, delete, replace (singular)
  * [ ] Global/multi replace
  * [x] Duplicate entry resolver  
    * [x] *(TODO tree compressed)*
    <!--
    * [x] Find duplicate entries
    * [x] Automatic double duplicate remover
    -->
  * [ ] Maintain metadata
  * [ ] Write metadata
  * [x] Entry validation
    * [x] *(TODO tree compressed)*
    <!--
    * [x] Check if steno contains illegal characters
    * [x] Check if steno is mechanically possible
    -->
* [ ] Create backup dictionary.
* [ ] Create alternate dictionaries targeting different standards of compliance.
* [ ] TUI?
* [ ] GUI
