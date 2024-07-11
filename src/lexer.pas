unit Lexer;

interface

const
  Specials: Array of AnsiString = ('(', ')', '{', '}', ':');

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
function LexerParseSymbol(var Lexer: TLexer): TSymbol;
function LexerExpectSymbols(var Lexer: TLexer; const ExpectedNames: Array of AnsiString): TSymbol;

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

function IsNotSpaceOrSpecial(const C: Char): Boolean;
var
  Special : AnsiString;
begin
  IsNotSpaceOrSpecial := True;

  if IsSpace(C) then Exit(False);

  for Special in Specials do
    if C = Special then
      Exit(False);
end;

function IsNotQuote(const C: Char): Boolean;
begin
  IsNotQuote := C <> '''';
end;

function IsNotNewline(const C: Char): Boolean;
begin
  IsNotNewline := C <> LineEnding;
end;

function LexerChopSymbol(var Lexer: TLexer): PSymbol;
var
  Loc : TLoc;
  Special : AnsiString;
  InsideQuotes : AnsiString;
  FoundSpecial : Boolean;
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
    Exit(Nil);

  Loc := LexerLoc(Lexer);
  New(LexerChopSymbol);

  if Lexer.Source[1] = '''' then
  begin
    LexerAdvanceLoc(Lexer, '''');
    Lexer.Source := Copy(Lexer.Source, 2, Length(Lexer.Source));
    InsideQuotes := LexerStripWhile(Lexer, @IsNotQuote);
    LexerChopSymbol^.Name := '''' + InsideQuotes + '''';
    LexerChopSymbol^.Loc := Loc;
    LexerAdvanceLoc(Lexer, '''');
    Lexer.Source := Copy(Lexer.Source, 2, Length(Lexer.Source));
  end
  else
  begin
    FoundSpecial := False;
    for Special in Specials do
      if LexerStripPrefix(Lexer, Special) then
      begin
        LexerChopSymbol^.Name := Special;
        LexerChopSymbol^.Loc := Loc;
        FoundSpecial := True;
        Exit(LexerChopSymbol);
      end;

    if not FoundSpecial then
    begin
      LexerChopSymbol^.Name := LexerStripWhile(Lexer, @IsNotSpaceOrSpecial);
      LexerChopSymbol^.Loc := Loc;
    end;
  end;
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

function LexerParseSymbol(var Lexer: TLexer): TSymbol;
var
  Symbol : PSymbol;
begin
  Symbol := LexerNextSymbol(Lexer);
  if Symbol <> nil then
    LexerParseSymbol := Symbol^
  else
  begin
    WriteLn(StdErr, LocToStr(LexerLoc(Lexer)), ': ERROR: Expected symbol but reached the end of the input');
    Halt(1);
  end;
end;

function LexerExpectSymbols(var Lexer: TLexer; const ExpectedNames: Array of AnsiString): TSymbol;
var
  Symbol : TSymbol;
  Buffer : AnsiString;
  I : LongWord;
begin
  Symbol := LexerParseSymbol(Lexer);
  for I := 0 to High(ExpectedNames) do
    if Symbol.Name = ExpectedNames[I] then
      Exit(Symbol);

  Buffer := '';
  for I := 0 to High(ExpectedNames) do
    if I = 0 then
      Buffer := Buffer + ExpectedNames[I]
    else if I = High(ExpectedNames) then
      Buffer := Buffer + ', or ' + ExpectedNames[I]
    else
      Buffer := Buffer + ', ' + ExpectedNames[I];

  WriteLn(StdErr, LocToStr(Symbol.Loc), ': ERROR: Expected ', Buffer, ' but got ', Symbol.Name);
  Halt(I);
end;

end.
