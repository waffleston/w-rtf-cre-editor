# w-rtf-cre-editor
A stenography RTF/CRE dictionary editor. 
## What is it?
It's a lightweight RTF/CRE dictionary editor, aiming to be as easy to use as possible without sacrificing advanced functionality.  There are a number of useful features, including:
* Duplicate entry detection & resolution
* Entry validation
* Dictionary searches (both strokes and translations)
## [Download](build/)
(Windows and GNU/Linux)  
It's currently a terminal-only program, but I'm working on some ways to bring it into the "modern age".

-----
[License](LICENSE) | [Contributing](CONTRIBUTING.md) | [Code of Conduct](CODE_OF_CONDUCT.md)

---

## In Progress:
* [x] Dictionary Editing (Cont.)
  * [ ] Global/multi replace
* [ ] Metadata
  * [x] Header
    * [x] Maintain it.
    * [x] Replace system text.
    * [ ] Read modified keyboard arrangement?  
    `RTF/CRE spec does not have this, is it Stenograph specific?`
    * [ ] Dates  
    `RTF/CRE spec says this is only for transcripts/notes.  Should it be supported anyways?`
* [ ] Provide as a library?
* [ ] GUI
  * [ ] Electron?
  * [ ] ncurses/blessed?
  * [ ] GTK/Qt?
* [ ] Create alternate dictionaries targeting different standards of compliance.
