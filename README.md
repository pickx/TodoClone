# TodoClone

!(/screenshot.png?raw=true)

I have attempted to replicate some of the functionality of Ultralist (https://ultralist.io), a command line todo manager.

In order to do this I have implemented the following:
1) Adding, deleting etc. of tasks via command line args.
2) Loading and writing of tasks to a file. Rather than use a serialization library I implemented a json-like plain-text writing.
3) Parsing of tasks using ReadP.

## Notes
- Task descriptions cannot contain a quote character, parsing it with my current system requires lookahead and I didn't feel like implementing it.

## Features to add in the future

- Basic refactoring: organize it into different files, reduce redundant code
- Implement more Ultralist features. Most importantly, filtering the list (filter by contexts, projects, completed, overdue...) and sorting the list.
