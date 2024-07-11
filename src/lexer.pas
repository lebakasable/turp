unit Lexer;

interface

type
  TLoc = record
    FilePath : AnsiString;
    Row, Col : LongWord;
  end;

  PSymbol = ^TSymbol;
  TSymbol = record
    Name : AnsiString;
    Loc : TLoc;
  end;
  TSymbolArray = Array of TSymbol;

  TCharPredicate = function(const C: Char): Boolean;
  
  TLexer = record
    Source : AnsiString;
    FilePath : AnsiString;
    Pos : LongWord;
    Bol : LongWord;
    Row : LongWord;
    Peek : PSymbol;
  end;

function LocToStr(const Loc: TLoc): AnsiString;
function CreateLexer(const Source, FilePath: AnsiString): TLexer;
function LexerLoc(const Lexer: TLexer): TLoc;
procedure LexerAdvanceLoc(var Lexer: TLexer; const SkippedChar: Char);
function LexerStripPrefix(var Lexer: TLexer; const Prefix: AnsiString): Boolean;
function LexerStripWhile(var Lexer: TLexer; const Skip: TCharPredicate): AnsiString;
function LexerChopSymbol(var Lexer: TLexer): PSymbol;
function LexerNextSymbol(var Lexer: TLexer): PSymbol;
function LexerPeekSymbol(var Lexer: TLexer): PSymbol;

implementation

function LocToStr(const Loc: TLoc): AnsiString;
var
  Row, Col : AnsiString;
begin
  Str(Loc.Row, Row);
  Str(Loc.Col, Col);
  LocToStr := Loc.FilePath + ':' + Row + ':' + Col;
end;

function CreateLexer(const Source, FilePath: AnsiString): TLexer;
begin
  CreateLexer.Source := Source;
  CreateLexer.FilePath := FilePath;
end;

function LexerLoc(const Lexer: TLexer): TLoc;
begin
  with LexerLoc do
  begin
    FilePath := Lexer.FilePath;
    Row := Lexer.Row + 1;
    Col := Lexer.Pos - Lexer.Bol + 1;
  end;
end;

procedure LexerAdvanceLoc(var Lexer: TLexer; const SkippedChar: Char);
begin
  Inc(Lexer.Pos);
  if SkippedChar = LineEnding then
  begin
    Lexer.Bol := Lexer.Pos;
    Inc(Lexer.Row);
  end;
end;

function StripPrefix(const Prefix, Str: AnsiString): AnsiString;
begin
  if Pos(Prefix, Str) = 1 then
    StripPrefix := Copy(Str, Length(Prefix) + 1, Length(Str) - Length(Prefix))
  else
    StripPrefix := Str;
end;

function LexerStripPrefix(var Lexer: TLexer; const Prefix: AnsiString): Boolean;
var
  Source : AnsiString;
  C : Char;
begin
  Source := StripPrefix(Prefix, Lexer.Source);
  if Source <> Lexer.Source then
  begin
    for C in Prefix do
      LexerAdvanceLoc(Lexer, C);
    Lexer.Source := Source;
    LexerStripPrefix := True;
  end
  else
    LexerStripPrefix := False;
end;

function LexerStripWhile(var Lexer: TLexer; const Skip: TCharPredicate): AnsiString;
var
  EndPos : LongWord;
  C : Char;
begin
  EndPos := 1;
  for C in Lexer.Source do
  begin
    if Skip(C) then
    begin
      LexerAdvanceLoc(Lexer, C);
      Inc(EndPos);
    end
    else Break;
  end;

  LexerStripWhile := Copy(Lexer.Source, 1, EndPos - 1);
  Lexer.Source := Copy(Lexer.Source, EndPos, Length(Lexer.Source) - EndPos + 1);
end;

function IsSpace(const C: Char): Boolean;
begin
  IsSpace := C in [' ', #9, #10, #13];
end;

function IsNotSpace(const C: Char): Boolean;
begin
  IsNotSpace := not IsSpace(C);
end;

function IsNotNewline(const C: Char): Boolean;
begin
  IsNotNewline := C <> LineEnding;
end;

function LexerChopSymbol(var Lexer: TLexer): PSymbol;
var
  Loc : TLoc;

  Special : Array of AnsiString = ('(', ')', '{', '}', ':');
  Name : AnsiString;
begin
  while True do
  begin
    LexerStripWhile(Lexer, @IsSpace);
    if LexerStripPrefix(Lexer, '//') then
      LexerStripWhile(Lexer, @IsNotNewline)
    else
      Break;
  end;

  if Length(Lexer.Source) = 0 then
    Exit(nil);

  Loc := LexerLoc(Lexer);

  New(LexerChopSymbol);

  for Name in Special do
    if LexerStripPrefix(Lexer, Name) then
    begin
      LexerChopSymbol^.Name := Name;
      LexerChopSymbol^.Loc := Loc;
      Exit(LexerChopSymbol);
    end;

  LexerChopSymbol^.Name := LexerStripWhile(Lexer, @IsNotSpace);
  LexerChopSymbol^.Loc := Loc;
end;

function LexerNextSymbol(var Lexer: TLexer): PSymbol;
begin
  if Lexer.Peek <> nil then
  begin
    LexerNextSymbol := Lexer.Peek;
    Lexer.Peek := nil;
  end
  else
    LexerNextSymbol := LexerChopSymbol(Lexer);
end;

function LexerPeekSymbol(var Lexer: TLexer): PSymbol;
begin
  if Lexer.Peek = nil then
    Lexer.Peek := LexerChopSymbol(Lexer);
  LexerPeekSymbol := Lexer.Peek;
end;

end.
