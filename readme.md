# Emmet engine for Delphi and Free Pascal

Emmet is mostly used by web-developers to simplify and speed up editing. Emmet can take
an abbreviation and expand it into a structured code block. Standard Emmet is written in
JavaScript and available in many text editors or web development tools. This is Emmet for Delphi
and Free Pascal. It is written from scratch and is used in RJ TextEd and CudaText applications.

## Getting Started

Download the files and include the Pascal files in your project.

### Usage

First you need to create an Emmet object.

```
FEmmet := TEmmet.Create(snippetsPath, loremPath);
```

* snippetsPath: The file path to Snippets.ini e.g. "c:\foo\Snipptes.ini"
* loremPath: The file path to Lorem.txt e.g. "c:\foo\Lorem.txt"

To expand an abbreviation, use

```
sExpanded := FEmmet.ExpandAbbreviation(sAbbr, sSyntax, sSelText, sSection, bMultiCursorTabs);
```

Parameters:

* **sAbbr**: Abbreviation e.g. "ul>li*5"
* **sSyntax**: Code language in lowercase e.g. "html". Available values are: html, css, xsl, svg, xml, jsx, less, sass, scss.
* **sSelText**: Text is used to wrap with abbreviation
* **sSection**: Gets the section used in snippets.ini e.g. "html"
* **bMultiCursorTabs**: Gets True if cursor positions in expanded string should be handled as multi cursor positions

Result: sExpanded is the resulting expanded code. It may contain cursor `|` positions or selected tab `${1:charset}` positions.

Using the overloaded version you can set some options.

```
var rOptions: TExpandOptions;
rOptions.Wordwrap := True;
rOptions.AddSlashToEmptyTags := False;
sExpanded := FEmmet.ExpandAbbreviation(sAbbr, sSyntax, sSelText, sSection, bMultiCursorTabs, rOptions);
```

### Wrap with abbreviation

To use this feature, use parameter sSelText. E.g. if you call ExpandAbbreviation() with:

* sAbbrev: `ul>li*`
* sSelText: 
```
Line 1
Line 2
Line 3
Line 4
```

You get the result

```
<ul>
  <li>Line 1</li>
  <li>Line 2</li>
  <li>Line 3</li>
  <li>Line 4</li>
</ul>
```

## Cheat sheets
* **Emmet-Pascal** - [Cheat sheet](https://www.rj-texted.se/Help/Emmetcheatsheet.html)
* **Standard Emmet** - [Cheat sheet](https://docs.emmet.io/cheat-sheet/)

## Authors

* **Rickard Johansson** - *Emmet.pas* - [RJ TextEd forum](https://www.rj-texted.se/Forum/index.php)
* **Alexey Torgashin** - *Misc files and demos* - [GitHub](https://github.com/Alexey-T)