(*--------------------------------------------------------------------------------------------
Unit Name: Emmet
Author:    Rickard Johansson  (https://www.rj-texted.se/Forum/index.php)
Date:      30-June-2020
Version:   1.18
Purpose:   Expand Emmet abbreviations and wrap selected text

Usage:
Create an Emmet object

    FEmmet := TEmmet.Create(snippetsFile, loremFile);

    snippetsFile      = The file path to snippets.ini e.g. "c:\foo\Snipptes.ini"
    loremFile         = The file path to Lorem.txt e.g. "c:\foo\Lorem.txt"

and call

    sExpanded := FEmmet.ExpandAbbreviation(sAbbr, sSyntax, sSelText, sSection, bMultiCursorTabs);

    sAbbr             = Abbreviation                               e.g. "ul>li*5"
    sSyntax           = Code language in lowercase                 e.g. "html"
    sSelText          = Text is used in wrap with abbreviation
    sSection          = Get the section used in snippets.ini       e.g. "html"
    bMultiCursorTabs  = True if cursor positions in expanded string should be
                    handled as multi cursor positions

    sExpanded is the resulting expanded code. It may contain cursor | positions or
    selected tab ${1:charset} positions.

Using the overloaded version you can set some expand options.

    var rOptions: TExpandOptions;
    rOptions.Wordwrap := True;
    rOptions.AddSlashToEmptyTags := False;
    sExpanded := FEmmet.ExpandAbbreviation(sAbbr, sSyntax, sSelText, sSection, bMultiCursorTabs, rOptions);


--------------------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------------------
Version updates and changes

Version 1.18
    * Single quotes in custom attributes should work now. E.g. td[title='Hello world!' colspan=3]

Version 1.17
    * Fixed a Lazarus (Free Pascal) string issue in ResolveTabStopsIndex()

Version 1.16
    * If the result string contain both cursor positions and tab stops - cursor positions
      are replaced by tab stops.

Version 1.15
    * Added option to replace cursor positions | with indexed tab positions ${x}

Version 1.14
    * Fixed ${1:tabstop} index issue
    * Fixed a class issue with span tag

Version 1.13
    * Added support for markdown. Supported abbreviations are:
      a           = link
      b           = bold
      bq          = blockquote
      code        = inline code snippet
      h1..h6      = heading
      hr          = horizontal rule
      i           = italic
      img         = image
      ol          = ordered list
      pre         = code block with language based highlighting
      strike      = strike through
      table       = table
      ul          = unordered list
      @l or @l80  = create lorem generated text

    * Added a property to TEmmet to set a maximum multiplication limit. The default value is cMultiplicationMax = 1000.

Version 1.12
    * Fixed a word wrap issue in Lorem generated text.

Version 1.11
    * The option "CommentTags" in TExpandOptions didn't work. The |c filter did however.
    * Added a number check in ProcessTagMultiplication() to prevent an infinite loop. (cMultiplicationMax = 1000)

Version 1.10
    * Removed "public" keyword from TExpandOptions.
    * Fixed an issue in ExtractFilters().

Version 1.09
    * TEmmet can now handle some filters. A filter is added at the end of the abbreviation using a pipe |.
      E.g. ul>li*|t

      c - Comment important tags (containing class or id attributes).
      e - Escape XML-unsafe characters: <, > and &. E.g. <p>|e => &lt;p&gt;&lt;/p&gt;
      s - Single line. Expand everything to a single line of code.
      t - Trim line markers from wrapped lines e.g. "* ", "- " or "1."
      w - Wordwrap selected or lorem generated text. Default width is 80.
      w<x> - Wordwrap at column x. E.g. |w120 will wrap lines at column 120.

      Ex.
        sAbbrev = ul>li*|t

        sSelText =
          * Line 1
          * Line 2

        Result =
          <ul>
            <li>Line 1</li>
            <li>Line 2</li>
          </ul>

    * Added several options in a record "TExpandOptions" that can be passed to the expand function.
      Options are:
        AddSlashToEmptyTags: Boolean;  // Add a slash to empty tags e.g. <img src="" />
        AlwaysAddNewLine: Boolean;     // Always add linefeed after each tag (usually used in XML)
        CommentTags: Boolean;          // Comment important tags (containing class or id attributes).
        IndentChilds: Boolean;         // Indent child tags. If you set this to false - no indention will be used.
        SingleLine: Boolean            // Expand everything to a single line of code.
        TabSize: Integer;              // Tab size in characters. This is only used with wordwrap.
        TrimLineMarkers: Boolean;      // Trim line markers from wrapped lines e.g. "* ", "- " or "1."
        Wordwrap: Boolean;             // Word wrap selected or lorem generated text.
        WordwrapAt: Integer;           // Wrap at given column. The nearest space or symbol will be used as wrap position.

    * Added overload function to "ExpandAbbreviation()". The first one uses default options and the other one
      enables you to set expand options.

Version 1.08
    * Direction issue with multiply.
    * Changes to the constructior.

Version 1.07
    * Added support for placeholders $# used in "Wrap with abbreviation".
      The placeholder is replaced with one line of selected text.

      E.g.
        sAbbrev = "ul>li[title=$#]*>{$#}+img[alt=$#]"

        sSelText =
           "About
            New
            Products
            Contacts"

        Result =
           "<ul>
              <li title="About">About<img src="" alt="About" /></li>
              <li title="New">New<img src="" alt="New" /></li>
              <li title="Products">Products<img src="" alt="Products" /></li>
              <li title="Contacts">Contacts<img src="" alt="Contacts" /></li>
           </ul>"

    * Added new parameters to the constructor.

Version 1.06
    * Space should be treated as stop character.
    * Implicit tag issue.
    * User attribute space issue.

Version 1.05
    * Fixed a child indent issue.

Version 1.04
    * Added standard vendor prefix "-" to CSS. E.g. -bdrs (which works the same as -v-bdrs).
    * Space issue with siblings.
    * A trim issue that may result in wrong indention.
    * Id and class attribute issue.

Version 1.03
    * Fixed several issues and updated the snippets.ini file.
Version 1.02
    * Addressed some warnings in Lazarus
Version 1.01
    * Fixed a multiply issue in ProcessTagMultiplication(...)
--------------------------------------------------------------------------------------------*)

unit Emmet;

interface

uses
  {$ifndef fpc}
  System.IniFiles,
  System.Classes;
  {$else}
  Classes, IniFiles;
  {$endif}

type
  TExpandOptions = record
    AddSlashToEmptyTags: Boolean; // Add a slash to empty tags e.g. <img src="" />
    AlwaysAddNewLine: Boolean;    // Always add linefeed after each tag (usually used in XML)
    AlwaysReturnIndexedTabStops: Boolean; // Convert all cursor positions | to indexed tab positions ${x} (HMTL,XML)
    CommentTags: Boolean;         // Comment important tags (containing class or id attributes).
    IndentChilds: Boolean;        // Indent child tags. If you set this to false - no indention will be used.
    SingleLine: Boolean;          // Expand everything to a single line of code.
    TabSize: Integer;             // Tab size in characters. This is only used with wordwrap.
    TrimLineMarkers: Boolean;     // Trim line markers from wrapped lines e.g. "* ", "- " or "1."
    Wordwrap: Boolean;            // Word wrap selected or lorem generated text.
    WordwrapAt: Integer;          // Wrap at given column. The nearest space or symbol will be used as wrap position.
  end;

  TEmmet = class(TObject)
  private
    FTagList: TStringList;
    FAbbreviations: TMemIniFile;
    FExpandOptions: TExpandOptions;
    FExtendedSyntax: Boolean;
    FExtends: string;
    FExtendsKey: string;
    FExtendsSnippetKey: string;
    FFilenameLorem: string;
    FFilenameSnippets: string;
    FLorem: TStringList;
    FFilters: TStringList;
    FMultiCursorTabs: Boolean;
    FMultiplicationMax: Integer;
    FRecursiveIndex: Integer;
    FSelection: string;
    FSyntax: string;
    FSyntaxKey: string;
    FSyntaxSnippetKey: string;
    FTabStopIndex: Integer;
    FTagInlineLevel: TStringList;
    function AddTag(s: string; const sAttribute, sId, sClass, sText: string; const
        nIndent: Integer): string;
    procedure AddToTagList(const s, sText: string);
    function CountLines(const s: string): Integer;
    function CreateLoremString(const nr: Integer): string;
    function ExpandCSSAbbrev(const AString: string): string;
    function ExpandTagAbbrev(sAbbrev: string; const nIndent: Integer = 0): string;
    function InsertUserAttribute(const s, sAttribute, sId, sClass: string): string;
    function IsTagInline(const s: string; var bText: Boolean): Boolean;
    function CreateTagAndClass(var s: string; const sClass: string): string;
    function ExpandMarkdownAbbrev(const AString: string): string;
    function ExtractFilters(s: string; const ASyntax: string): string;
    function ExtractUserAttributes(const sAttribute: string): string;
    function FormatSelection(const s: string; const ind: Integer): string;
    function HasTabStopsAndCursors(const src: string): Boolean;
    function InsertLoremString(const sub, s: string; const indent: Integer; const bRemoveBrackets: Boolean = False): string;
    function InsertSelection(s: string; const bOneLine: Boolean = False): string;
    function PostProcessFilters(s: string): string;
    function ProcessTagAbbrev(const AString: string; const index, len, indent:
        Integer): string;
    function ProcessTagGroup(const AString: string; const index: Integer; out ipos:
        Integer; const indent: Integer): string;
    function ProcessTagMultiplication(const AString: string; const index, indent:
        Integer): string;
    procedure ResolveCursorPositions(var src: string);
    function ResolveTabStopsIndex(s: string): string;
  public
    constructor Create(const ASnippetsFile, ALoremFile: string); overload;
    destructor Destroy; override;
    function ExpandAbbreviation(AString: string; const ASyntax, ASelText: string;
        out ASection: string; out bMultiCursorTabs: Boolean): string; overload;
    function ExpandAbbreviation(AString: string; const ASyntax, ASelText: string;
        out ASection: string; out bMultiCursorTabs: Boolean; const opt:
        TExpandOptions): string; overload;
    function GetAbbreviationNames(const ASyntax: string; const AList: TStringList):
        Boolean;
    function GetSnippetNames(const ASyntax: string; const AList: TStringList):
        Boolean;
    property MultiplicationMax: Integer read FMultiplicationMax write FMultiplicationMax;
  end;

implementation

uses
  {$ifndef fpc}
  System.SysUtils, Vcl.Dialogs, System.Math, Winapi.Windows, System.StrUtils;
  {$else}
  SysUtils, Dialogs, Math, StrUtils;
  {$endif}

{$ifdef fpc}
// Compatability with FPC 3.0.4
function Pos(const SubStr, Str: string; Offset: integer=1): integer; inline;
begin
  Result := PosEx(SubStr, Str, Offset);
end;
{$endif}

const
  cInlineLevel = 'a,abbr,acronym,applet,b,basefont,bdo,big,br,button,cite,code,del,dfn,em,font,i,iframe,img,input,ins,kbd,label,map,object,q,s,samp,select,small,span,strike,strong,sub,sup,textarea,tt,u,var';
  cMultiplicationMax = 1000;


constructor TEmmet.Create(const ASnippetsFile, ALoremFile: string);
begin
  inherited Create;

  FMultiplicationMax := cMultiplicationMax;

  FFilenameSnippets := ASnippetsFile;
  FFilenameLorem := ALoremFile;

  FAbbreviations := TMemIniFile.Create(FFilenameSnippets);

  FFilters := TStringList.Create;
  FFilters.Delimiter := '|';

  FTagList := TStringList.Create;
  FTagInlineLevel := TStringList.Create;
  FTagInlineLevel.Delimiter := ',';
  FTagInlineLevel.DelimitedText := cInlineLevel;

  FLorem := TStringList.Create;
end;

destructor TEmmet.Destroy;
begin
  inherited;
  FAbbreviations.Free;
  FFilters.Free;
  FTagList.Free;
  FTagInlineLevel.Free;
  FLorem.Free;
end;

function TEmmet.AddTag(s: string; const sAttribute, sId, sClass, sText: string;
    const nIndent: Integer): string;
var
  w,st: string;
  n: Integer;

  function ResolveEmptyTag: string;
  var
    w: string;
  begin
    if FTagList.Count > 0 then
    begin
      w := FTagList[FTagList.Count-1];
      if w <> '' then
        w := Copy(w,3,Length(w)-3);
    end;
    if FAbbreviations.ValueExists('elementmap', w) then
    begin
      Result := FAbbreviations.ReadString('elementmap', w, '');
    end
    else if not FExpandOptions.AlwaysAddNewLine and (FTagInlineLevel.IndexOf(w) >= 0) then
    begin
      Result := 'span';
    end
    else
    begin
      Result := 'div';
    end;
  end;

begin
  if (s <> '') or (sId <> '') or (sClass <> '') or (sAttribute <> '') then
  begin
    w := '';
    if FExpandOptions.IndentChilds then
      st := StringOfChar(#9,nIndent)
    else
      st := '';
    if sId <> '' then
    begin
      if (s = '') then s := ResolveEmptyTag;
      w := '<' + s + #32 + sId;
    end;
    if sClass <> '' then
    begin
      if w <> '' then
      begin
        if not FExpandOptions.AlwaysAddNewLine and (FTagInlineLevel.IndexOf(s) >= 0) then
          w := w + '>' + '<span ' + sClass + '></span>'
        else
          w := w + #32 + sClass;
      end
      else
        w := CreateTagAndClass(s,sClass);
    end;
    if sAttribute <> '' then
    begin
      if (s = '') then s := ResolveEmptyTag;
      if w <> '' then
      begin
        n := Pos(s,w);
        if n > 0 then
          Insert(ExtractUserAttributes(sAttribute), w, n + Length(s))
        else if (w[Length(w)] = '>') then
          Insert(ExtractUserAttributes(sAttribute), w, Length(w))
        else
          w := w + ExtractUserAttributes(sAttribute) + '>';
      end
      else
        w := '<' + s + ExtractUserAttributes(sAttribute) + '>';
    end;

    if w = '' then
      w := '<' + s + '>'
    else if (w <> '') and (w[Length(w)] <> '>') then
      w := w + '>';
    FTagList.Add('</'+s+'>');
    Result := st + w + sText;
  end
  else
  begin
    Result := sText;
  end;
end;

procedure TEmmet.AddToTagList(const s, sText: string);
var
  n: Integer;
  w: string;
begin
  if Pos('/>', s) = 0 then
  begin
    n := Pos(#32, s);
    if n = 0 then n := Pos('>', s);
    if n > 0 then
      w := Copy(s, 2, n - 2)
    else
      w := Copy(s, 2, Length(s) - 2);
    FTagList.Add(sText+'</'+w+'>');
  end;
end;

function TEmmet.CountLines(const s: string): Integer;
var
  n,m: Integer;
begin
  Result := 1;
  n := Pos(#13#10, s);
  m := n + 2;
  if n > 0 then
  begin
    if n = 1 then Dec(Result);
    while n > 0 do
    begin
      n := Pos(#13#10, s, m);
      if n > 0 then m := n + 2;
      Inc(Result);
    end;
    if m > Length(s) then Dec(Result);
  end;
end;

function TEmmet.CreateLoremString(const nr: Integer): string;
var
  s,sz: string;
  x,z,ln: Integer;
  bFirst: Boolean;
begin
  Result := 'Lorem Ipsum ';
  if FLorem.Count <= 0 then
  begin
    sz := FFilenameLorem;
    if FileExists(sz) then
      FLorem.LoadFromFile(sz);
  end;
  bFirst := False;
  sz := ',.;!?';
  ln := Length(Result);
  for x := 2 to nr - 1 do
  begin
    s := FLorem[Random(FLorem.Count - 1)];
    if bFirst and (Ord(S[1]) > 90) then
    begin
      s[1] := Char(Ord(s[1]) - 32);
      bFirst := False;
    end;

    if (Random(32767) mod 60 < 8) and (x < nr -1) then
    begin
      z := Random(5) + 1;
      s := s + sz[z];
      bFirst := z > 1;
    end;
    Result := Result + s + ' ';
    ln := ln + Length(s) + 1;
  end;
  Result := Trim(Result) + '.';
end;

function TEmmet.ExpandAbbreviation(AString: string; const ASyntax, ASelText:
    string; out ASection: string; out bMultiCursorTabs: Boolean; const opt:
    TExpandOptions): string;
var
  typ: string;
  n: Integer;
begin
  FExpandOptions := opt;
  FTagList.Clear;
  FRecursiveIndex := 0;
  FSelection := ASelText;
  bMultiCursorTabs := False;
  FTabStopIndex := 0;
  FMultiCursorTabs := False;

  AString := ExtractFilters(AString, ASyntax);

  FSyntax := ASyntax;
  if (ASyntax = 'xslt') then FSyntax := 'xsl';

  FExtends := FAbbreviations.ReadString(FSyntax,'extends','');
  typ := FAbbreviations.ReadString(FSyntax,'type','');

  FSyntaxKey := 'abbreviations-' + FSyntax;
  FExtendsKey := 'abbreviations-' + FExtends;
  FSyntaxSnippetKey := 'snippets-' + FSyntax;
  FExtendsSnippetKey := 'snippets-' + FExtends;

  if (typ = 'xml') then
  begin
    Result := ExpandTagAbbrev(AString);
    Result := PostProcessFilters(Result);
  end
  else if (typ = 'css') then
    Result := ExpandCSSAbbrev(AString)
  else if (typ = 'md') then
    Result := ExpandMarkdownAbbrev(AString)
  else
    Result := AString;

  if FExpandOptions.AlwaysReturnIndexedTabStops or HasTabStopsAndCursors(Result) then
    ResolveCursorPositions(Result);

  bMultiCursorTabs := FMultiCursorTabs;

  if FExtendedSyntax then
    ASection := FExtends
  else
    ASection := FSyntax;
end;

function TEmmet.ExpandAbbreviation(AString: string; const ASyntax, ASelText:
    string; out ASection: string; out bMultiCursorTabs: Boolean): string;
var
  opt: TExpandOptions;
begin
  if ASyntax = 'xml' then
    opt.AlwaysAddNewLine := True
  else
    opt.AlwaysAddNewLine := False;
  opt.AddSlashToEmptyTags := True;
  opt.CommentTags := False;
  opt.IndentChilds := True;
  opt.SingleLine := False;
  opt.TabSize := 2;
  opt.TrimLineMarkers := True;
  opt.Wordwrap := False;
  opt.AlwaysReturnIndexedTabStops := False;
  opt.WordwrapAt := 80;

  Result := ExpandAbbreviation(AString, ASyntax, ASelText, ASection, bMultiCursorTabs, opt);
end;

function TEmmet.ExpandCSSAbbrev(const AString: string): string;
var
  sVendor,sAbbrev: string;

  function ExtractVendor(const s: string; var sv: string): string;
  var
    n: Integer;
  begin
    Result := s;
    sv := '';
    if s[1] <> '-' then Exit;

    n := Pos('-',s,2);
    if n = 0 then n := 1;
    sv := Copy(s,1,n);
    Result := Copy(Result,n+1,Length(Result));
  end;

  function ProcessVendor(const sv, s: string): string;
  begin
    Result := s;

    if (sv = '-') or (sv = '-v-') then
    begin
      FMultiCursorTabs := True;
      Result := '-webkit-' + s + #13#10;
      Result := Result + '-moz-' + s + #13#10;
      Result := Result + s;
    end
    else if sv = '-w-' then
    begin
      FMultiCursorTabs := True;
      Result := '-webkit-' + s + #13#10;
      Result := Result + s;
    end
    else if sv = '-m-' then
    begin
      FMultiCursorTabs := True;
      Result := '-moz-' + s + #13#10;
      Result := Result + s;
    end;
  end;

begin
  Result := '';
  if AString = '' then Exit;

  sAbbrev := ExtractVendor(AString, SVendor);

  if FAbbreviations.ValueExists(FSyntaxSnippetKey,sAbbrev) then
  begin
    Result := FAbbreviations.ReadString(FSyntaxSnippetKey,sAbbrev,'');
    FExtendedSyntax := False;
  end
  else if FAbbreviations.ValueExists(FExtendsSnippetKey,sAbbrev) then
  begin
    Result := FAbbreviations.ReadString(FExtendsSnippetKey,sAbbrev,'');
    FExtendedSyntax := True;
  end;

  if Result = '' then Exit;

  if sVendor <> '' then
  begin
    Result := ProcessVendor(sVendor, Result);
    Result := StringReplace(Result, ':', ': ', [rfReplaceAll]);
  end
  else
  begin
    Result := StringReplace(Result, ':', ': ', []);
  end;

  Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
end;

function TEmmet.ExpandTagAbbrev(sAbbrev: string; const nIndent: Integer = 0):
    string;
var
  indx,npos,ind: Integer;
  ch: Char;
  indent: Integer;
  tagListCount: Integer;
  s,sw,w: string;
  bInline,bText: Boolean;

  function SkipToEndBrace(const s: string; i: Integer; const chStart, chEnd: Char): Integer;
  var
    n,np: Integer;
  begin
    Result := i;
    np := 1;
    Inc(i);
    n := i;
    while (i <= Length(s)) and (np > 0) do
    begin
      if s[i] = chStart then
        Inc(np)
      else if s[i] = chEnd then
        Dec(np);
      Inc(i);
    end;

    if (i > n) and (np = 0) then
      Result := i - 1;
  end;

  function AddToString(const s, sAdd: string): string;
  var
    i: Integer;
  begin
    Result := s;
    if Length(sAdd) = 0 then Exit;

    if Length(s) = 0 then
    begin
      Result := sAdd;
      Exit;
    end;

    if (Length(s) > 0) and (s[Length(s)] = #10) then
    begin
      Result := s + sAdd;
      Exit;
    end;

    i := 1;
    while (i <= Length(sAdd)) and CharInSet(sAdd[i], [#9,#32,#160]) do Inc(i);

    if (i <= Length(sAdd)) and (sAdd[i] = '<') then
      Result := s + Trim(sAdd)
    else
      Result := s + sAdd;
  end;

  function InsertTabPoint(const s: string): string;
  var
    i: Integer;
  begin
    Result := '';
    i := Length(s);
    if i = 0 then Exit;
    if s[i] <> '>' then Exit;

    while (i > 0) and (s[i] <> '<') do Dec(i);
    Inc(i);

    if s[i] = '/' then Exit;

    Result := '|';
  end;

  function AddChild(const s, sw: string): string;
  var
    w: string;
    bText: Boolean;
  begin
    Result := '';
    w := ExpandTagAbbrev(s,indent);
    if IsTagInline(w,bText) then
    begin
      if bText then
        Result := Result + sw + w
      else
        Result := Result + sw + Trim(w);
      bInline := True;
    end
    else
    begin
      Result := Result + sw + #13#10;
      Result := Result + w;
    end;
    if (Length(Result) > 0) and (Result[Length(Result)] <> #10) and not IsTagInline(w,bText) then
      Result := Result + #13#10;
  end;

  function AddSibling(const s: string): string;
  var
    w: string;
  begin
    Result := '';
    if FTagList.Count > tagListCount then
    begin
      w := FTagList[FTagList.Count-1];
      if bInline then
        Result := Result + InsertTabPoint(s) + w
      else
        Result := Result + InsertTabPoint(s) + w + #13#10;
      FTagList.Delete(FTagList.Count-1);
    end
    else if indx <> npos then
    begin
      if (w = '') and not bInline then
        Result := Result + #13#10;
    end;
  end;

  function ClimbUpOneLevel(const s: string): string;
  var
    w: string;
    bText: Boolean;
  begin
    Result := s;
    while (FTagList.Count > 0) and (FTagList.Count >= tagListCount) do
    begin
      w := FTagList[FTagList.Count-1];
      if IsTagInline(w,bText) then
        Result := Result + InsertTabPoint(s) + w
      else
        Result := Result + InsertTabPoint(s) + w + #13#10;
      FTagList.Delete(FTagList.Count-1);
      if FExpandOptions.IndentChilds then Dec(indent);
    end;
  end;

  function AddExpanded(const s: string): string;
  var
    w: string;
    bText: Boolean;
  begin
    Result := ProcessTagAbbrev(s,npos,indx-npos,indent);
    if FTagList.Count > tagListCount then
    begin
      w := FTagList[FTagList.Count-1];
      FTagList.Delete(FTagList.Count-1);
      if bInline or IsTagInline(w,bText) then
        Result := Result + InsertTabPoint(Result) + w
      else
        Result := Result + InsertTabPoint(Result) + w + #13#10;
    end;
  end;

  function AddEndTags: string;
  var
    w,st: string;
    bText: Boolean;
  begin
    Result := '';
    while (FTagList.Count > 0) and (FTagList.Count > tagListCount) do
    begin
      if FExpandOptions.IndentChilds then
        st := StringOfChar(#9,indent)
      else
        st := '';
      w := FTagList[FTagList.Count-1];
      if bInline then
      begin
        Result := Result + w;
        if not IsTagInline(w,bText) then
          Result := Result + #13#10;
        bInline := False;
      end
      else
      begin
        if not IsTagInline(w,bText) then
          w := st + w;
        if (Length(Result) > 0) and (Result[Length(Result)] <> #10) and not IsTagInline(w,bText) then
          Result := Result + #13#10 + w
        else
          Result := Result + w;
      end;
      FTagList.Delete(FTagList.Count-1);
      if FExpandOptions.IndentChilds then Dec(indent);
    end;
  end;

  function CanIndent(const sa: string; const index: Integer): Boolean;
  var
    w: string;
  begin
    Result := True;
    if FSyntax = 'xml' then Exit;
    w := 'html';
    Result := (Pos(w,sa,index) <> index);
  end;

begin
  Result := '';
  Inc(FRecursiveIndex);
  sAbbrev := StringReplace(sAbbrev, '\n', #13#10, [rfReplaceAll]);
  sAbbrev := StringReplace(sAbbrev, '\t', #9, [rfReplaceAll]);

  if FExpandOptions.IndentChilds then
    indent := nIndent
  else
    indent := 0;

  bInline := False;
  tagListCount := FTagList.Count;
  npos := 1;
  indx := 1;
  s := '';
  while indx <= Length(sAbbrev) do
  begin
    ch := sAbbrev[indx];
    case ch of
      '(': // group
      begin
        if indx > npos then
        begin
          s := s + ProcessTagAbbrev(sAbbrev,npos,indx-npos,indent);
          npos := indx;
        end;
        s := s + ProcessTagGroup(sAbbrev,npos,indx,indent);
        npos := indx + 1;
      end;

      '>': // child operator
      begin
        ind := indent;
        if indx > npos then
        begin
          w := ProcessTagAbbrev(sAbbrev,npos,indx-npos,indent);
        end;
        if FExpandOptions.IndentChilds and CanIndent(sAbbrev,npos) then
          Inc(indent);
        npos := indx + 1;

        Inc(indx);
        s := s + AddChild(Copy(sAbbrev,indx,Length(sAbbrev)), w);
        npos := Length(sAbbrev) + 1;
        indx := npos;
        if FExpandOptions.IndentChilds then
          indent := ind;
      end;

      '+': // sibling operator
      begin
        if indx = Length(sAbbrev) then Inc(indx);
        if indx > npos then
        begin
          sw := ProcessTagAbbrev(sAbbrev,npos,indx-npos,indent);
          bInline := IsTagInline(sw,bText);
          if bInline and not bText then
            s := s + Trim(sw)
          else
            s := s + sw;
        end;
        s := s + AddSibling(s);
        npos := indx + 1;
      end;

      '^': // climb up operator
      begin
        if indx > npos then
        begin
          sw := ProcessTagAbbrev(sAbbrev,npos,indx-npos,indent);
          if IsTagInline(sw,bText) and not bText then
            s := s + Trim(sw)
          else
            s := s + sw
        end;
        s := ClimbUpOneLevel(s);
        while (indx + 1 < Length(sAbbrev)) and (sAbbrev[indx+1] = '^') do
        begin
          Dec(tagListCount);
          s := ClimbUpOneLevel(s);
          Inc(indx);
        end;
        if FExpandOptions.IndentChilds then
          Inc(indent);
        npos := indx + 1;
      end;

      '*': // multiplication operator
      begin
        Inc(indx);
        while (indx <= Length(sAbbrev)) and CharInSet(sAbbrev[indx], ['0'..'9']) do Inc(indx);
        if (indx <= Length(sAbbrev)) and (sAbbrev[indx] = '>') then
        begin
          Inc(indx);
          while (indx <= Length(sAbbrev)) and not CharInSet(sAbbrev[indx], ['>','^']) do Inc(indx);
        end;
        s := s + ProcessTagMultiplication(sAbbrev,npos,indent);
        npos := indx;
        Dec(indx);
      end;

      '{': // skip to ending }
      begin
        indx := SkipToEndBrace(sAbbrev, indx, '{', '}');
      end;

      '[': // skip to ending ]
      begin
        indx := SkipToEndBrace(sAbbrev, indx, '[', ']');
      end;

      '"': // skip to ending "
      begin
        indx := SkipToEndBrace(sAbbrev, indx, '"', '"');
      end;

      #32: // space is a stop character and shouldn't be found here (wrap up and exit)
      begin
        Result := s + AddEndTags;
        Exit;
      end;
    end;
    Inc(indx);
    if (npos < indx) and (npos <= Length(sAbbrev)) and (indx > Length(sAbbrev)) then
    begin
      s := AddToString(s,AddExpanded(sAbbrev));
      if FExpandOptions.IndentChilds then
        Dec(indent);
    end;
  end;
  s := s + AddEndTags;

  Dec(FRecursiveIndex);
  if (FRecursiveIndex = 0) and (FSelection <> '') then
    Result := InsertSelection(s)
  else
    Result := s;

  if (FTagList.Count = 0) and (indent = 0) then
    Result := Trim(Result);

  if not FExpandOptions.AddSlashToEmptyTags then
  begin
    Result := StringReplace(Result, ' />', '>', [rfReplaceAll]);
    Result := StringReplace(Result, '/>', '>', [rfReplaceAll]);
  end;
end;

function TEmmet.InsertUserAttribute(const s, sAttribute, sId, sClass: string):
    string;
var
  i: Integer;
  sa: string;
  ch: Char;

  procedure InsertAttribute(const sa: string; var s: string; index: Integer);
  var
    sn: string;
    n,m: Integer;
  begin
    n := Pos('=',sa);
    if n > 0 then
    begin
      sn := Copy(sa,1,n) + '"';
      m := Pos(sn,s);
      if m > 0 then
      begin
        n := Pos('"',s,m+Length(sn));
        if n > m then
        begin
          index := m;
          Delete(s,m,n-m+1);
        end;
      end;
    end;
    Insert(sa,s,index);
  end;

begin
  Result := s;
  if (sAttribute = '') and (sId = '') and (sClass = '') then Exit;
  i := 1;
  while i <= Length(s) do
  begin
    ch := s[i];
    if CharInSet(ch, ['/','>','+','^','(']) then
    begin
      sa := ExtractUserAttributes(sAttribute);
      if (sa <> '') then
        InsertAttribute(sa,Result,i);
      if (sId <> '') then
        Insert(#32 + sId,Result,i);
      if (sClass <> '') then
        Insert(#32 + sClass,Result,i);
      Exit;
    end;
    Inc(i);
  end;
end;

function TEmmet.IsTagInline(const s: string; var bText: Boolean): Boolean;
var
  w: string;
  m,n,k: Integer;
begin
  Result := True;
  if Length(s) = 0 then Exit;

  bText := True;
  m := Pos('<',s,1);
  if m = 0 then Exit;
  bText := False;

  if (Length(s) > m) and (s[m+1] = '/') then Inc(m);

  n := Pos(#32,s,m+1);
  k := Pos('>',s,m+1);
  if (n > 0) and (k > 0) and (k < n) then n := k;
  if (n = 0) and (k > 0) then n := k;
  k := Pos('/>',s,m+1);
  if (n > 0) and (k > 0) and (k < n) then n := k;
  if (n = 0) and (k > 0) then n := k;

  if n = 0 then n := Length(s) + 1;
  w := Copy(s,m+1,n-m-1);

  Result := not FExpandOptions.AlwaysAddNewLine and (FTagInlineLevel.IndexOf(w) >= 0);
end;

function TEmmet.ProcessTagAbbrev(const AString: string; const index, len,
    indent: Integer): string;
var
  s,sn,st: string;
  sValue: string;
  sText,sId,sClass,sAttr: string;
  ls: TStringList;
  bDone: Boolean;

  function ExtractText(const s: string; out sText: string): string;
  var
    n,np,i: Integer;
  begin
    Result := s;
    n := Pos('{', Result);
    if (n > 0) then
    begin
      while (n > 1) and (Result[n-1] = '$') do
        n := Pos('{', Result, n + 1);

      if n = 0 then Exit;

      np := 1;
      i := n + 1;
      while (i <= Length(Result)) and (np > 0) do
      begin
        if Result[i] = '{' then
          Inc(np)
        else if Result[i] = '}' then
          Dec(np);
        Inc(i);
      end;

      if (i > n) and (np = 0) then
      begin
        sText := Copy(Result,n+1,i-n-2);
        Delete(Result,n,i-n);
      end;
    end;
    if Pos('@lorem',sText) > 0 then
      sText := InsertLoremString('@lorem',sText,indent);
  end;

  function ExtractAttribute(const s: string; out sa: string): string;
  var
    n,m: Integer;
  begin
    n := Pos('[', s);
    if (n > 0) then
    begin
      Result := Copy(s,1,n-1);
      m := Pos(']', s);
      if m > 0 then
        sa := Copy(s,n+1,m-n-1);
    end
    else
    begin
      Result := s;
      sa := '';
    end;
  end;

  function ExtractIdAndClass(const s: string; var sd, sc: string): string;
  var
    n,m: Integer;
    w,ws: string;
  begin
    Result := s;
    n := Pos('#', s);
    m := Pos('.', s);
    if (n > 0) or (m > 0) then
    begin
      if (n > 0) and (m > 0) then
        n := Min(n,m)
      else
        n := Max(n,m);

      m := n;
      while (n <= Length(s)) and not CharInSet(s[n], [#9,#32,'^','>','+','*','{','[','(']) do Inc(n);
      w := Copy(s,m,n-m);
      Delete(Result,m,n-m);

      // Extract id
      n := Pos('#',w);
      if n > 0 then
      begin
        sd := 'id="';
        while n > 0 do
        begin
          m := n;
          Inc(n);
          while (n <= Length(w)) and not CharInSet(w[n], ['#','.']) do Inc(n);
          ws := Copy(w,m+1,n-m-1);
          sd := sd + ws + #32;
          Delete(w,m,n-m);
          n := Pos('#',w);
        end;
        sd := Trim(sd) + '"';
      end;

      // Extract class
      n := Pos('.',w);
      if n > 0 then
      begin
        sc := 'class="';
        while n > 0 do
        begin
          m := n;
          Inc(n);
          while (n <= Length(w)) and not CharInSet(w[n], ['#','.']) do Inc(n);
          ws := Copy(w,m+1,n-m-1);
          sc := sc + ws + #32;
          Delete(w,m,n-m);
          n := Pos('.',w);
        end;
        sc := Trim(sc) + '"';
      end;
    end
    else
    begin
      sd := '';
      sc := '';
    end;
  end;

  function GetSnippet(const sn: string): string;
  begin
    Result := '';
    if FAbbreviations.ValueExists(FSyntaxSnippetKey,sn) then
    begin
      Result := FAbbreviations.ReadString(FSyntaxSnippetKey,sn,'');
      Result := ResolveTabStopsIndex(Result);
      FExtendedSyntax := False;
    end;

    if Result <> '' then Exit;

    if FAbbreviations.ValueExists(FExtendsSnippetKey,sn) then
    begin
      Result := FAbbreviations.ReadString(FExtendsSnippetKey,sn,'');
      Result := ResolveTabStopsIndex(Result);
      FExtendedSyntax := True;
    end;
  end;

  function ExistsAbbreviation(const sn: string): Boolean;
  begin
    Result := FAbbreviations.ValueExists(FSyntaxKey, sn);
    if not Result then
      Result := FAbbreviations.ValueExists(FExtendsKey, sn)
  end;

  function GetAbbreviation(const sn: string): string;
  begin
    Result := '';
    FExtendedSyntax := False;
    Result := FAbbreviations.ReadString(FSyntaxKey, sn, '');
    if Result = '' then
    begin
      Result := FAbbreviations.ReadString(FExtendsKey, sn, '');
      FExtendedSyntax := True;
    end;
    Result := ResolveTabStopsIndex(Result);
  end;

begin
  Result := '';
  bDone := False;
  s := Trim(Copy(AString,index,len));

  // Snippet?
  sValue := GetSnippet(s);
  if sValue <> '' then
  begin
    Result := sValue;
    Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
    Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
    Exit;
  end;

  // Get text
  s := ExtractText(s, sText);

  // Get attribute
  sn := ExtractAttribute(s, sAttr);

  // Get id and class
  sn := ExtractIdAndClass(sn, sId, sClass);

  if ExistsAbbreviation(sn) then
  begin
    if FExpandOptions.IndentChilds then
      st := StringOfChar(#9,indent)
    else
      st := '';
    ls := TStringList.Create;
    try
      sn := GetAbbreviation(sn);
      if (Length(sn) > 0) and (sn[1] <> '<') then
      begin
        sn := ExpandTagAbbrev(sn, indent);
        st := '';
        bDone := True;
      end;
    finally
      ls.Free;
    end;
    if (Length(sn) > 0) and (sn[1] = '<') then
    begin
      Result := st + InsertUserAttribute(sn,sAttr,sId,sClass) + sText;
      Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
      Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
      if not bDone then AddToTagList(sn,'');
      Exit;
    end;
    Result := st + sn + sText;
    Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
    Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
    if not bDone then AddToTagList(sn,'');
    Exit;
  end;

  if (sAttr <> '') or (sId <> '') or (sClass <> '') then
  begin
    Result := AddTag(sn,sAttr,sId,sClass,sText,indent);
    Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
    Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
    Exit;
  end;

  // Tag?
  if (Length(s) > 0) and (s[1] = '<') then
  begin
    Result := s;
    AddToTagList(s,sText);
    Exit;
  end;

  Result := AddTag(s,'','','',sText,indent);
  Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
end;

function TEmmet.CreateTagAndClass(var s: string; const sClass: string): string;
var
  w: string;
begin
  Result := '';
  if s = '' then
  begin
    if FTagList.Count > 0 then
    begin
      w := FTagList[FTagList.Count-1];
      if w <> '' then
        w := Copy(w,3,Length(w)-3);
    end;
    if FAbbreviations.ValueExists('elementmap', w) then
    begin
      s := FAbbreviations.ReadString('elementmap', w, '');
      Result := '<' + s + #32 + sClass + '>';
    end
    else if not FExpandOptions.AlwaysAddNewLine and (FTagInlineLevel.IndexOf(w) >= 0) then
    begin
      s := 'span';
      Result := '<span ' + sClass + '>'
    end
    else
    begin
      s := 'div';
      Result := '<div ' + sClass + '>';
    end;
  end
  else
  begin
    if not FExpandOptions.AlwaysAddNewLine and (FTagInlineLevel.IndexOf(s) >= 0) and (s <> 'span') then
      Result := '<' + s + '>' + '<span ' + sClass + '></span>'
    else
      Result := '<' + s + #32 + sClass + '>';
  end;
end;

function TEmmet.ExpandMarkdownAbbrev(const AString: string): string;
var
  sAbbrev: string;
begin
  Result := '';
  if AString = '' then Exit;

  sAbbrev := AString;

  if FAbbreviations.ValueExists(FSyntaxSnippetKey,sAbbrev) then
  begin
    Result := FAbbreviations.ReadString(FSyntaxSnippetKey,sAbbrev,'');
    FExtendedSyntax := False;
  end
  else if FAbbreviations.ValueExists(FExtendsSnippetKey,sAbbrev) then
  begin
    Result := FAbbreviations.ReadString(FExtendsSnippetKey,sAbbrev,'');
    FExtendedSyntax := True;
  end;

  if (Result <> '') and (Pos('@l',Result) > 0) then
    Result := InsertLoremString('@l',Result,0)
  else if (Result = '') and (Pos('@l',sAbbrev) > 0) then
    Result := InsertLoremString('@l',sAbbrev,0);

  if Result = '' then Exit;

  Result := StringReplace(Result, ':', ': ', []);
  Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9, [rfReplaceAll]);
end;

function TEmmet.ExtractFilters(s: string; const ASyntax: string): string;
var
  i: Integer;
  ws,wf: string;
begin
  Result := s;
  wf := FAbbreviations.ReadString(ASyntax,'filters','');
  i := Pos('|',s);
  if (i = 0) and (wf = '') then Exit;

  if i > 0 then
    Result := Copy(s,1,i-1);
  if wf <> '' then wf := '|' + wf;
  FFilters.DelimitedText := Copy(s,i+1,Length(s)-i) + wf;

  // Find trim list marker filter |t
  i := FFilters.IndexOf('t');
  if i >= 0 then
  begin
    FExpandOptions.TrimLineMarkers := True;
    FFilters.Delete(i);
  end;

  // Find wordwrap filter |w or |w120
  i := 0;
  while i < FFilters.Count do
  begin
    ws := FFilters[i];
    if (ws <> '') and (ws[1] = 'w') then
    begin
      FFilters.Delete(i);
      FExpandOptions.Wordwrap := True;
      if Length(ws) > 1 then
      begin
        ws := Copy(ws,2,Length(ws)-1);
        FExpandOptions.WordwrapAt := StrToIntDef(ws,FExpandOptions.WordwrapAt);
      end;
      Break;
    end;
    Inc(i);
  end;
end;

function TEmmet.ExtractUserAttributes(const sAttribute: string): string;
var
  i,n: Integer;
  sn,sa: string;
  ls: TStringList;
begin
  Result := '';
  if sAttribute = '' then Exit;

  ls := TStringList.Create;
  try
    i := Pos(#32,sAttribute);
    if i > 0 then
    begin
      n := 1;
      i := 1;
      while (i <= Length(sAttribute)) do
      begin
        if sAttribute[i] = '"' then
        begin
          i := Pos('"',sAttribute,i+1);
          if i > 0 then
          begin
            ls.Add(Copy(sAttribute,n,i-n+1));
            Inc(i);
            n := i+1;
          end;
        end
        else if sAttribute[i] = #39 then
        begin
          i := Pos(#39,sAttribute,i+1);
          if i > 0 then
          begin
            ls.Add(Copy(sAttribute,n,i-n+1));
            Inc(i);
            n := i+1;
          end;
        end
        else if sAttribute[i] = #32 then
        begin
          ls.Add(Copy(sAttribute,n,i-n));
          n := i+1;
        end
        else if i = Length(sAttribute) then
        begin
          ls.Add(Copy(sAttribute,n,i-n+1));
          n := i+1;
        end;
        Inc(i);
      end;
    end
    else
    begin
      ls.Add(sAttribute);
    end;

    for i := 0 to ls.Count - 1 do
    begin
      sn := ls[i];
      if Pos('=',sn) = 0 then
        sa := ''
      else
        sa := ls.ValueFromIndex[i];
      if (sa <> '') and (sa[1] = #39) then
        sa := StringReplace(sa, #39, '"', [rfReplaceAll])
      else if (sa = '') then
        sa := '""'
      else if (sa <> '') and (sa[1] <> '"') then
        sa := '"' + sa + '"';

      sn := ls.Names[i];
      if sn = '' then sn := ls[i];
      Result := Result + #32 + sn + '=' + sa;
    end;
  finally
    ls.Free;
  end;
end;

function TEmmet.FormatSelection(const s: string; const ind: Integer): string;
var
  n,m,len: Integer;
  w,ws,wt: string;
begin
  Result := '';

  wt := StringOfChar(#9, ind);
  if FExpandOptions.Wordwrap then
  begin
    len := FExpandOptions.WordwrapAt - ind * FExpandOptions.TabSize;
    w := StringReplace(s, #13#10, #32, [rfReplaceAll]);
    n := 1;
    m := 1;
    while n <= Length(w) do
    begin
      if Length(w) - m + 1 > len then
      begin
        // Wrap text
        n := m + len;
        while (n > m) and not CharInSet(w[n],[#9,#32,#160,',','.',')','>',':',']','}']) do Dec(n);
        ws := wt + Trim(Copy(w,m,n-m+1));
        Result := Result + ws + #13#10;
        m := n + 1;
      end
      else
      begin
        ws := wt + Trim(Copy(w,m,Length(w)-m+1));
        Result := Result + ws + #13#10;
        Break;
      end;
    end;
  end
  else
  begin
    m := 1;
    n := Pos(#13#10, s);
    while m > 0 do
    begin
      if n > 0 then
      begin
        w := Trim(Copy(s,m,n-m+1));
        if w <> '' then
          Result := Result + wt + w + #13#10;
        m := n + 2;
      end
      else
      begin
        w := Trim(Copy(s,m,Length(s)));
        if w <> '' then
          Result := Result + wt + w + #13#10;
        Exit;
      end;
      n := Pos(#13#10, s, m);
    end;
  end;
end;

function TEmmet.GetAbbreviationNames(const ASyntax: string; const AList:
    TStringList): Boolean;
var
  sa,se: string;
  ls: TStringList;
begin
  ls := TStringList.Create;
  try
    sa := 'abbreviations-' + ASyntax;
    se := FAbbreviations.ReadString(ASyntax,'extends','');
    FAbbreviations.ReadSection(sa,ls);
    AList.AddStrings(ls);
    if se <> '' then
    begin
      se := 'abbreviations-' + se;
      FAbbreviations.ReadSection(se,ls);
      AList.AddStrings(ls);
    end;
  finally
    ls.Free;
  end;
  Result := AList.Count > 0;
end;

function TEmmet.GetSnippetNames(const ASyntax: string; const AList:
    TStringList): Boolean;
var
  sc,se: string;
  ls: TStringList;
begin
  ls := TStringList.Create;
  try
    sc := 'snippets-' + ASyntax;
    se := FAbbreviations.ReadString(ASyntax,'extends','');
    FAbbreviations.ReadSection(sc,ls);
    AList.AddStrings(ls);
    if se <> '' then
    begin
      se := 'snippets-' + se;
      FAbbreviations.ReadSection(se,ls);
      AList.AddStrings(ls);
    end;
  finally
    ls.Free;
  end;
  Result := AList.Count > 0;
end;

function TEmmet.HasTabStopsAndCursors(const src: string): Boolean;
begin
  Result := (FTabStopIndex > 0) and (Pos('|',src) > 0);
end;

function TEmmet.InsertLoremString(const sub, s: string; const indent: Integer; const bRemoveBrackets: Boolean = False):
    string;
var
    w,ws,wr,wt,wn: string;
    n,m,nr,len: Integer;
    slen: Integer;
  begin
    Result := s;
    nr := 30;
    m := Pos(sub,s);

    if m = 0 then Exit;

    slen := Length(sub);
    n := m + slen;
    while (n <= Length(s)) and CharInSet(s[n], ['0'..'9']) do Inc(n);
    if n > m + slen then
    begin
      wn := Copy(s,m+slen,n-m-slen);
      nr := StrToIntDef(wn, 30);
    end;
    w := CreateLoremString(nr);

    if FExpandOptions.Wordwrap then
    begin
      wt := '';
      wr := '';
      wt := StringOfChar(#9, indent);
      len := FExpandOptions.WordwrapAt - indent * FExpandOptions.TabSize;
      n := 1;
      m := 1;
      while n <= Length(w) do
      begin
        if Length(w) - m + 1 > len then
        begin
          // Wrap text
          n := m + len;
          while (n > m) and not CharInSet(w[n],[#9,#32,#160,',','.',')','>',':',']','}']) do Dec(n);
          ws := Trim(Copy(w,m,n-m+1));
          if wr <> '' then wr := wr + wt;
          wr := wr + ws + #13#10;
          m := n + 1;
        end
        else
        begin
          ws := Trim(Copy(w,m,Length(w)-m+1));
          if wr <> '' then wr := wr + wt;
          wr := wr + ws;
          Break;
        end;
        len := FExpandOptions.WordwrapAt - Length(wt) * FExpandOptions.TabSize;
      end;
      w := wr;
    end;

    if bRemoveBrackets then
      Result := StringReplace(s,'{' + sub + wn + '}',w,[])
    else
      Result := StringReplace(s,sub+wn,w,[]);
end;

function TEmmet.InsertSelection(s: string; const bOneLine: Boolean = False):
    string;
var
  i,n,ind: Integer;
  ws,wt: string;
  wsel: string;

  function TrimLine(const s: string): string;
  var
    i: Integer;
  begin
    i := 1;
    if FExpandOptions.TrimLineMarkers then
    begin
      Result := Trim(s);
      if (Length(Result) > 0) and CharInSet(Result[1], ['0'..'9']) then
      begin
        // Make sure we have a list marker like "1."
        while (i <= Length(Result)) and CharInSet(Result[i], ['0'..'9']) do Inc(i);
        if (i <= Length(Result)) and (Result[i] = '.') then
          Inc(i)
        else
          i := 1;
      end
      else if (Length(Result) > 0) and CharInSet(Result[1], ['*','-']) then
      begin
        // Make sure the line starts with "* " or "- "
        Inc(i);
        if (i <= Length(Result)) and (Result[i] = #32) then
          Inc(i)
        else
          i := 1;
      end;
      Result := Trim(Copy(Result,i,Length(Result)));
    end
    else
      Result := Trim(Copy(s,i,Length(s)));
  end;

  function GetFirstLine: string;
  var
    n: Integer;
  begin
    n := Pos(#13#10, FSelection);
    if n = 0 then
    begin
      Result := TrimLine(FSelection);
      FSelection := '';
      Exit;
    end;
    Result := TrimLine(Copy(FSelection, 1, n-1));
    Delete(FSelection, 1, n+1);
    while (Result = '') and (FSelection <> '') do
    begin
      n := Pos(#13#10, FSelection);
      if n > 0 then
      begin
        Result := TrimLine(Copy(FSelection, 1, n-1));
        Delete(FSelection, 1, n+1);
      end
      else
      begin
        Result := TrimLine(FSelection);
        FSelection := '';
      end;
    end;
  end;
begin
  s := StringReplace(s,'|','',[rfReplaceAll]);
  Result := s;
  if bOneLine then
    wsel := GetFirstLine
  else
    wsel := FSelection;

  n := Pos('$#',s);
  if n > 0 then
  begin
    // Replace placeholders $# with selected text
    Result := StringReplace(s,'$#',wsel,[rfReplaceAll]);
    Exit;
  end;

  n := 0;
  ind := 1;
  i := Length(s);
  while i > 0 do
  begin
    if (s[i] = '>') then n := i;
    if (s[i] = '<') and (s[i+1] = '/') then
    begin
      ind := 1;
      n := 0;
    end
    else if (s[i] = '<') and (s[i+1] <> '/') then
    begin
      ind := 1;
      while (i > 1) and (s[i-1] = #9) do
      begin
        Inc(ind);
        Dec(i);
      end;
      Break;
    end;
    Dec(i);
  end;
  if n > 0 then
  begin
    if bOneLine then
      Result := Copy(s,1,n) + wsel + Copy(s,n+1,Length(s))
    else
    begin
      wt := StringOfChar(#9, ind-1);
      ws := #13#10 + FormatSelection(wsel, ind);
      Result := Copy(s,1,n) + ws + wt + Copy(s,n+1,Length(s));
    end;
  end;
end;

function TEmmet.PostProcessFilters(s: string): string;
var
  i: Integer;
  ws: string;

  function ProcessEscape(const s: string): string;
  const
    slt = '&lt;';
    sgt = '&gt;';
    samp = '&amp;';
  var
    i,j: Integer;
  begin
    SetLength(Result, Length(s)*4);
    j := 1;
    for i := 1 to Length(s) do
    begin
      case s[i] of
        '<': begin
          Move(slt[1], Result[j], Length(slt) * 2);
          Inc(j,Length(slt));
        end;
        '>': begin
          Move(sgt[1], Result[j], Length(sgt) * 2);
          Inc(j,Length(sgt));
        end;
        '&': begin
          Move(samp[1], Result[j], Length(samp) * 2);
          Inc(j,Length(samp));
        end;
        else
        begin
          Result[j] := s[i];
          Inc(j);
        end;
      end;
    end;
    SetLength(Result, j-1);
  end;

  function ExtractAttributeValuesAndTag(const s: string; var sTag: string; var nPos: Integer): string;
  var
    i,n,m,ntag: Integer;
    ws,sId,sClass: string;
  begin
    Result := '';
    sTag := '';
    nPos := 0;
    ntag := 0;

    i := Pos('id=',s);
    if i > 0 then
    begin
      m := Pos('"',s,i);
      if m > 0 then
      begin
        n := Pos('"',s,m+1);
        if n > 0 then
        begin
          sId := Copy(s,m+1,n-m-1);
          ntag := i;
        end;
      end;
    end;

    i := Pos('class=',s);
    if i > 0 then
    begin
      m := Pos('"',s,i);
      if m > 0 then
      begin
        n := Pos('"',s,m+1);
        if n > 0 then
        begin
          sClass := Copy(s,m+1,n-m-1);
          ntag := i;
        end;
      end;
    end;

    if (sId = '') and (sClass = '') then Exit;

    // Extract tag
    i := ntag;
    while (i > 0) and (s[i] <> '<') do Dec(i);
    if i > 0 then
    begin
      nPos := i+1;
      n := Pos(#32,s,i);
      if n > 0 then
        sTag := Copy(s,i+1,n-i-1);
    end;

    // Create comment tag
    ws := '<!-- /';
    if sId <> '' then
      ws := ws + '#' + sId;
    if sClass <> '' then
      ws := ws + '.' + sClass;

    Result := ws + ' -->';
  end;

  procedure FindAndInsertComment(const ls: TStringList; const npos, index: Integer; const sTag, sComment: string);
  var
    s,st: string;
    i,x,np: Integer;
  begin
    np := 1;
    i := index;
    x := nPos + 1;
    while i < ls.Count do
    begin
      s := ls[i];
      x := Pos(sTag,s,x);
      if x > 0 then
      begin
        if s[x-1] = '/' then
          Dec(np)
        else
          Inc(np);
      end;

      if np = 0 then
      begin
        np := 1;
        while (np <= Length(s)) and (s[np] = #9) do Inc(np);
        st := StringOfChar(#9,np-1);
        if i+1 = ls.Count then
          ls.Add(st+sComment)
        else
          ls.Insert(i+1,st+sComment);
        Break;
      end;

      x := 1;
      Inc(i);
    end;
  end;

  function ProcessComments(const s: string): string;
  var
    ls: TStringlist;
    i,npos: Integer;
    ws,sTag,sComment: string;
  begin
    Result := '';
    ls := TStringList.Create;
    try
      ls.Text := s;
      for i := 0 to ls.Count - 1 do
      begin
        ws := ls[i];
        sComment := ExtractAttributeValuesAndTag(ws, sTag, npos);
        if (sComment <> '') and (sTag <> '') then
          FindAndInsertComment(ls,npos,i,sTag,sComment);
      end;
      Result := ls.Text;
    finally
      ls.Free;
    end;
  end;

  function ProcessSingleLine(const s: string): string;
  var
    ls: TStringlist;
    i: Integer;
  begin
    Result := '';
    ls := TStringList.Create;
    try
      ls.Text := s;
      for i := 0 to ls.Count - 1 do
      begin
        Result := Result + Trim(ls[i]);
      end;
    finally
      ls.Free;
    end;
  end;
begin
  Result := s;
  if FExpandOptions.SingleLine then
    Result := ProcessSingleLine(Result);
  if FExpandOptions.CommentTags then
    Result := ProcessComments(Result);
  if FFilters.Count = 0 then Exit;

  for i := 0 to FFilters.Count - 1 do
  begin
    ws := FFilters[i];
    if ws = '' then Continue;

    case ws[1] of
      'e': Result := ProcessEscape(Result);
      'c': Result := ProcessComments(Result);
      's': begin
        if not FExpandOptions.SingleLine then
          Result := ProcessSingleLine(Result);
      end;
    end;
  end;
end;

function TEmmet.ProcessTagGroup(const AString: string; const index: Integer;
    out ipos: Integer; const indent: Integer): string;
var
  np: Integer;
  TagListCount: Integer;
begin
  Result := '';
  TagListCount := FTagList.Count;
  ipos := index + 1;
  np := 1;
  while (ipos <= Length(AString)) and (np > 0) do
  begin
    if AString[ipos] = '(' then
      Inc(np)
    else if AString[ipos] = ')' then
      Dec(np);
    Inc(ipos);
  end;


  if (ipos <= Length(AString)) and (AString[ipos] = '*') then
  begin
    // multiply group
    Inc(ipos);
    while (ipos <= Length(AString)) and CharInSet(AString[ipos], ['0'..'9']) do Inc(ipos);
    Result := Result + ProcessTagMultiplication(AString,index,indent);
  end
  else
  begin
    Result := Result + ExpandTagAbbrev(Copy(AString,index+1,ipos-index-2),indent);
  end;

  if (FTagList.Count > 0) and (FTagList.Count > TagListCount) then
  begin
    Result := Result + FTagList[FTagList.Count-1];
    FTagList.Delete(FTagList.Count-1);
  end;

  if (Length(Result) > 0) and (Result[Length(Result)] <> #10) then
    Result := Result + #13#10;

  Dec(ipos);
end;

function TEmmet.ProcessTagMultiplication(const AString: string; const index,
    indent: Integer): string;
var
  i,n,num,numlen: Integer;
  nStart,nIndex,nInc: Integer;
  s,w: string;
  bAddSelection: Boolean;

  function GetExpression(const ws: string; n, i: Integer): string;
  var
    np: Integer;
  begin
    np := 0;
    if (i > 0) and (ws[i] = ')') then
    begin
      Dec(i);
      np := -1;
      while (i >= index) and (np < 0) do
      begin
        if ws[i] = '(' then
          Inc(np)
        else if ws[i] = ')' then
          Dec(np);
        Dec(i);
      end;
      Result := Copy(ws,i+1,n-i-1);
    end
    else
    begin
      while (i > 0) do
      begin
        if CharInSet(ws[i], ['{']) then Inc(np);
        if CharInSet(ws[i], ['}']) then Dec(np);
        if (np = 0) and CharInSet(ws[i], ['@']) then n := i;
        if CharInSet(ws[i], ['>','^']) then Break;
        Dec(i);
      end;
      Result := Copy(ws,i+1,n-i-1);
    end;
  end;

  function GetNumber(const ws: string; n, i: Integer; var nrlen: Integer; out astart, ainc: Integer): Integer;
  var
    sn: string;
  begin
    Result := 0;

    // Multiplier
    while (i <= Length(ws)) and CharInSet(ws[i], ['0'..'9']) do Inc(i);
    sn := Copy(ws,n+1,i-n-1);
    if sn <> '' then
      Result := StrToInt(sn);

    nrlen := Length(sn);

    if (Result = 0) and (FSelection <> '') then
    begin
      Result := CountLines(FSelection);
      bAddSelection := True;
    end;

    // Start and direction
    i := n - 1;
    while (i > 0) and CharInSet(ws[i], ['0'..'9']) do Dec(i);
    sn := Copy(ws,i+1,n-i-1);
    if ws[i] = '-' then
    begin
      if sn <> '' then
        astart := StrToInt(sn) + Result - 1
      else
        astart := Result;
      ainc := -1;
    end
    else
    begin
      if sn <> '' then
        astart := StrToInt(sn);
    end;
  end;

  function ReplaceVariables(const s: string; const nr: Integer): string;
  var
    sn,w: string;
    i,n: Integer;
  begin
    Result := s;
    n := Pos('$',Result);
    while n > 0 do
    begin
      i := n + 1;
      while (i <= Length(Result)) and (Result[i] = '$') do Inc(i);
      w := StringOfChar('$',i-n);
      if i-n > 1 then
        sn := Format('%.*d',[i-n, nr])
      else
        sn := IntToStr(nr);
      Result := StringReplace(Result,w,sn,[]);
      n := Pos('$',Result);
    end;
  end;

begin
  Result := '';
  bAddSelection := False;
  n := Pos('*',AString);
  nInc := 1;
  nStart := 1;

  // Get expression
  i := n - 1;
  s := GetExpression(AString,n,i);

  // Get number
  i := n + 1;
  num := GetNumber(AString,n,i,numlen,nStart,nInc);
  Inc(i,numlen);

  // Is number larger than the max value allowed? (FMultiplicationMax = 1000)
  if num > FMultiplicationMax then
    num := FMultiplicationMax;

  // handle '>' after number
  if (i <= Length(AString)) and (AString[i] = '>') then
  begin
    n := i;
    Inc(i);
    while (i <= Length(AString)) and not CharInSet(AString[i], ['>','^']) do Inc(i);
    s := s + Copy(AString,n,i-n);
  end;

  if num > 0 then
  begin
    nIndex := nStart;
    while num > 0 do
    begin
      w := ExpandTagAbbrev(s,indent);
      if bAddSelection then
        w := InsertSelection(w, True);
      w := ReplaceVariables(w, nIndex);
      if (Length(Result) > 0) and (Result[Length(Result)] <> #10) then
        Result := Result + #13#10 + w
      else
        Result := Result + w;
      Dec(num);
      Inc(nIndex,nInc);
    end;
  end;
end;

procedure TEmmet.ResolveCursorPositions(var src: string);
var
  n,nr: Integer;
  sn: string;
begin
  nr := FTabStopIndex;
  n := Pos('|',src);
  while n > 0 do
  begin
    Inc(nr);
    sn := '${' + IntToStr(nr) + '}';
    src := StringReplace(src, '|', sn, []);
    n := Pos('|',src);
  end;
end;

function TEmmet.ResolveTabStopsIndex(s: string): string;
var
  i,n,m,l: Integer;
  index,nr: Integer;
  sn,sm: string;
  ps,pr: PChar;

  procedure ResolveAllWithSameIndexA(var src: string; var str: string; const sf, sr: string; ns: Integer);
  var
    il,ln: Integer;
    psl,prl: PChar;
  begin
    ln := PosEx(sf, src, ns);
    while ln > 0 do
    begin
      // Change $ => # in source string to prevent it from being processed again
      src[ln] := '#';

      // Replace tab stop index in result string "str"
      psl := PChar(@sr[1]);
      prl := PChar(@str[ln]);
      for il := 1 to Length(sr) do
      begin
        prl^ := psl^;
        Inc(psl);
        Inc(prl);
      end;
      ln := PosEx(sf, src, ln+1);
    end;
  end;

begin
  Result := s;
  UniqueString(Result); // This is needed in Lazarus (Free Pascal)

  nr := 0;
  index := 0;
  n := Pos('${', s);
  while n > 0 do
  begin
    m := PosEx('}', s, n);
    l := PosEx(':', s, n);
    if (l > n) and (l < m) then
    begin
      // Tab stop index found
      Inc(n,2);
      sn := Copy(s,n,l-n);
      index := StrToInt(sn);

      // Replace index with FTabStopIndex + index
      Inc(nr);
      sm := IntToStr(FTabStopIndex + index);
      if (Length(sm) = Length(sn)) and (sm <> sn) then
      begin
        // Replace index by modify string in memory
        ps := PChar(@sm[1]);
        pr := PChar(@Result[n]);
        for i := 1 to Length(sm) do
        begin
          pr^ := ps^;
          Inc(ps);
          Inc(pr);
        end;
        ResolveAllWithSameIndexA(s, Result, '${'+sn, '${'+sm, m+1);
      end;
    end;
    n := PosEx('${', s, m+1);
  end;
  Inc(FTabStopIndex, nr);
end;

end.
