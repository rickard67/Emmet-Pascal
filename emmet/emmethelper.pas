unit EmmetHelper;

interface

uses
  Math;

procedure EmmetFindAbbrev(const S: UnicodeString; CurOffset: integer;
  out StartOffset: integer; out Abbrev: UnicodeString);

implementation

function IsCharWord(ch: Widechar): boolean;
begin
  case ch of
    'a'..'z',
    'A'..'Z',
    '0'..'9',
    '_':
      Result:= true;
    else
      Result:= false;
  end;
end;

function IsCharSpace(ch: WideChar): boolean;
begin
  Result:= (ch=' ') or (ch=#9);
end;

function EmmetOffsetIsTagEnd(const S: UnicodeString; N: integer): boolean;
begin
  Result:= false;
  if N<=1 then exit;
  if N>Length(S) then exit;
  if S[N]<>'>' then exit;

  Dec(N);
  if N<=1 then exit;
  if not IsCharWord(S[N]) then exit;
  while (N>0) and IsCharWord(S[N]) do Dec(N);

  if N<1 then exit;
  if S[N]='/' then
  begin
    Dec(N);
    if N<1 then exit;
  end;

  if S[N]<>'<' then exit;
  Result:= true;
end;

procedure EmmetFindAbbrev(const S: UnicodeString; CurOffset: integer;
  out StartOffset: integer; out Abbrev: UnicodeString);
var
  Found: boolean;
  ch: WideChar;
  N: integer;
  bBrackets, bBracketsSq: boolean;
begin
  StartOffset:= CurOffset;
  Abbrev:= '';
  Found:= false;

  CurOffset:= Min(CurOffset, Length(S));
  N:= CurOffset+1;
  bBrackets:= false;
  bBracketsSq:= false;

  repeat
    Dec(N);

    if N=0 then
    begin
      StartOffset:= 0;
      Abbrev:= Copy(S, StartOffset+1, CurOffset-StartOffset);
      Found:= true;
      Break;
    end;

    ch:= S[N];

    if ch='}' then
    begin
      bBrackets:= true;
      Continue;
    end;

    if ch='{' then
    begin
      bBrackets:= false;
      Continue;
    end;

    if ch=']' then
    begin
      bBracketsSq:= true;
      Continue;
    end;

    if ch='[' then
    begin
      bBracketsSq:= false;
      Continue;
    end;

    //quotes are not allowed in snippet name
    //(but can be inside [] brackets)
    if (ch='"') or (ch='''') then
      if not bBrackets and not bBracketsSq then
        exit;

    if IsCharSpace(ch) then
      if not bBrackets and not bBracketsSq then
      begin
        StartOffset:= N;
        Abbrev:= Copy(S, StartOffset+1, CurOffset-StartOffset);
        Found:= true;
        Break;
      end;

    if ch='>' then
      if EmmetOffsetIsTagEnd(S, N) then
      begin
        StartOffset:= N;
        Abbrev:= Copy(S, StartOffset+1, CurOffset-StartOffset);
        Found:= true;
        Break;
      end;
  until false;

  if Found then
    while (StartOffset<Length(S)) and IsCharSpace(S[StartOffset+1]) do
    begin
      Inc(StartOffset);
      Delete(Abbrev, 1, 1);
    end;
end;

end.
