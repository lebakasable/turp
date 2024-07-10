uses
  SysUtils;

type
  TLoc = record
    FilePath : AnsiString;
    Row, Col : LongWord;
  end;

function LocToStr(const Loc: TLoc): AnsiString;
begin
  LocToStr := Format('%s:%d:%d', [Loc.FilePath, Loc.Row, Loc.Col]);
end;

type
  PSymbol = ^TSymbol;
  TSymbol = record
    Name : AnsiString;
    Loc : TLoc;
  end;
  TSymbolArray = Array of TSymbol;
  
  TLexer = record
    Source : AnsiString;
    FilePath : AnsiString;
    Pos : LongWord;
    Bol : LongWord;
    Row : LongWord;
    Peek : PSymbol;
  end;

function CreateLexer(const Source, FilePath: AnsiString): TLexer;
begin
  CreateLexer.Source := Source;
  CreateLexer.FilePath := FilePath;
  with CreateLexer do
  begin
    Pos := 0;
    Bol := 0;
    Row := 0;
    Peek := nil;
  end;
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

type
  TCharPredicate = function(const C: Char): Boolean;

function LexerStripWhile(var Lexer: TLexer; const Skip: TCharPredicate): AnsiString;
var
  I, EndPos : LongWord;
begin
  EndPos := 1;
  for I := 1 to Length(Lexer.Source) do
  begin
    if Skip(Lexer.Source[I]) then
      Inc(EndPos)
    else
      Break;
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

function LexerChopSymbol(var Lexer: TLexer): PSymbol;
var
  Loc : TLoc;

  Special : Array of AnsiString = ('(', ')', '{', '}', ':');
  Name : AnsiString;
begin
  LexerStripWhile(Lexer, @IsSpace);

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

type
  TCase = record
    State : TSymbol;
    Read  : TSymbol;
    Write : TSymbol;
    Step  : TSymbol;
    Next  : TSymbol;
  end;
  TCaseArray = Array of TCase;

  TMachine = record
    State       : TSymbol;
    Tape        : TSymbolArray;
    TapeDefault : TSymbol;
    Head        : LongWord;
    Halt        : Boolean;
  end;

procedure MachineNext(var Machine: TMachine; const Cases: TCaseArray);
var
  Case_ : TCase;
begin
  for Case_ in Cases do
    if (Case_.State.Name = Machine.State.Name) and (Case_.Read.Name = Machine.Tape[Machine.Head].Name) then
    begin
      Machine.Tape[Machine.Head] := Case_.Write;
      case Case_.Step.Name of
        '<-':
        begin
          if Machine.Head = 0 then
          begin
            WriteLn(StdErr, LocToStr(Case_.Step.Loc), ': ERROR: Tape underflow');
            Halt(1);
          end;
          Dec(Machine.Head);
        end;
        '->':
        begin
          Inc(Machine.Head);
          if Machine.Head >= Length(Machine.Tape) then
            Insert(Machine.TapeDefault, Machine.Tape, Length(Machine.Tape));
        end;
      end;
      Machine.State := Case_.Next;
      Machine.Halt := False;
      Exit;
    end;
end;

procedure MachinePrint(const Machine: TMachine);
var
  Buffer : AnsiString;
  Head : LongWord;
  I : LongWord;
begin
  Buffer := Machine.State.Name + ': ';
  for I := 0 to High(Machine.Tape) do
  begin
    if I = Machine.Head then
      Head := Length(Buffer);
    Buffer := Buffer + Machine.Tape[I].Name + ' ';
  end;
  WriteLn(Buffer);
  for I := 0 to Head do Write(' ');
  WriteLn('^');
end;

function ParseSymbol(var Lexer: TLexer): TSymbol;
var
  Symbol : PSymbol;
begin
  Symbol := LexerNextSymbol(Lexer);
  if Symbol <> nil then
    ParseSymbol := Symbol^
  else
  begin
    WriteLn(StdErr, LocToStr(LexerLoc(Lexer)), ': ERROR: Expected symbol but reached the end of the input');
    Halt(1);
  end;
end;

function ParseStep(var Lexer: TLexer): TSymbol;
var
  Symbol : TSymbol;
begin
  Symbol := ParseSymbol(Lexer);
  case Symbol.Name of
    '->', '<-': ParseStep := Symbol;
  else
    WriteLn(StdErr, LocToStr(Symbol.Loc), ': ERROR: Expected -> or <- but got ', Symbol.Name);
    Halt(1);
  end;
end;

function ParseCase(var Lexer: TLexer): TCase;
begin
  with ParseCase do
  begin
    State := ParseSymbol(Lexer);
    Read  := ParseSymbol(Lexer);
    Write := ParseSymbol(Lexer);
    Step  := ParseStep(Lexer);
    Next  := ParseSymbol(Lexer);
  end;
end;

function ParseCases(var Lexer: TLexer): TCaseArray;
var
  Cases : TCaseArray = ();
begin
  while LexerPeekSymbol(Lexer) <> nil do
    Insert(ParseCase(Lexer), Cases, Length(Cases));
  ParseCases := Cases;
end;

function ParseTape(var Lexer: TLexer): TSymbolArray;
var
  Tape : TSymbolArray = ();
begin
  while LexerPeekSymbol(Lexer) <> nil do
    Insert(ParseSymbol(Lexer), Tape, Length(Tape));
  ParseTape := Tape;
end;

function SlurpFile(const FilePath: AnsiString): AnsiString;
var
  FileHandle : TextFile;
  Line : String;
begin
  Assign(FileHandle, FilePath);
  Reset(FileHandle);

  SlurpFile := '';
  while not Eof(FileHandle) do
  begin
    ReadLn(FileHandle, Line);
    SlurpFile := SlurpFile + Line + LineEnding;
  end;

  Close(FileHandle);

  if Length(SlurpFile) > 0 then
    SetLength(SlurpFile, Length(SlurpFile) - Length(LineEnding));
end;

procedure Usage(var Out: Text);
begin
  WriteLn(Out, 'Usage: turp <input.turp> <input.tape>');
end;

var
  TurpPath, TapePath : AnsiString;
  TurpSource, TapeSource : AnsiString;
  TurpLexer, TapeLexer : TLexer;

  Cases : TCaseArray;
  Tape : TSymbolArray;

  State, TapeDefault : TSymbol;

  Machine : TMachine;
begin
  if ParamCount = 0 then
  begin
    Usage(StdErr);
    WriteLn(StdErr, 'ERROR: No input.turp is provided');
    Halt(1);
  end;
  TurpPath := ParamStr(1);

  if ParamCount = 1 then
  begin
    Usage(StdErr);
    WriteLn(StdErr, 'ERROR: No input.tape is provided');
    Halt(1);
  end;
  TapePath := ParamStr(2);

  TurpSource := SlurpFile(TurpPath);
  TurpLexer := CreateLexer(TurpSource, TurpPath);
  Cases := ParseCases(TurpLexer);

  if Length(Cases) > 0 then
    State := Cases[0].State
  else
  begin
    WriteLn(StdErr, 'ERROR: The turp file must at least contain one case');
    Halt(1);
  end;

  TapeSource := SlurpFile(TapePath);
  TapeLexer := CreateLexer(TapeSource, TapePath);
  Tape := ParseTape(TapeLexer);

  if Length(Tape) > 0 then
    TapeDefault := Tape[High(Tape)]
  else
  begin
    WriteLn(StdErr, 'ERROR: The tape file must at least contain one symbol');
    Halt(1);
  end;

  Machine.State := State;
  Machine.Tape := Tape;
  Machine.TapeDefault := TapeDefault;

  while not Machine.Halt do
  begin
    MachinePrint(Machine);
    Machine.Halt := True;
    MachineNext(Machine, Cases);
  end;
end.
