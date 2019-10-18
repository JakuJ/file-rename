# file-rename

A console line utility to rename all files in current directory according to a supplied format string.

## Description

Format string syntax consists of any characters excluding '{' and '}', and special identifiers. For now, the supported identifiers include:

* {name} - old name
* {number} - sequence number
* {date} - today's date

File extensions are preserved. 

The program will halt if changing a name would overwrite another file.

## Installation

* Clone this repository

* Install Stack if you haven't yet done so
  
* Run `stack install` in the repo's folder

## Usage

`rename -vhe --dry-run [-p pattern] [format string]`

## Examples

Say we start with a folder contatining these files:
* abc.jpg
* def.pdf
* ghi.xml

We would get these results:

* `rename {name}` -> [abc.jpg, def.pdf, ghi.xml] (no change)
* `rename file_{number}` -> [file_1.jpg, file_2.pdf, file_3.xml]
* `rename -e {name}_{number}` -> [abc_1, def_2, ghi_3]
* `rename "{name} ({date})"` -> "abc (2019-10-16).jpg" and so on