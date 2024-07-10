uses
  SysUtils,
  StrUtils,
  Types;

type
  TStep = (Left, Right);

  TState = AnsiString;
  TSymbol = AnsiString;
  TSymbolArray = array of TSymbol;

  TTurp = record
    Current : TState;
    Read    : TSymbol;
    Write   : TSymbol;
    Step    : TStep;
    Next    : TState;
  end;

  TMachine = record
    Tape : TSymbolArray;
    Head : LongWord;
    State : TState;
  end;

function MachineNext(var Machine: TMachine; const Turps: array of TTurp): Boolean;
var
  Turp : TTurp;
begin
  for Turp in Turps do
    if (Turp.Current = Machine.State) and (Turp.Read = Machine.Tape[Machine.Head]) then
    begin
      Machine.Tape[Machine.Head] := Turp.Write;
      case Turp.Step of
        Left:
          if Machine.Head = 0 then
            Machine.Head := High(Machine.Tape)
          else
            Dec(Machine.Head);
        Right:
          Machine.Head := (Machine.Head + 1) mod Length(Machine.Tape);
      end;
      Machine.State := Turp.Next;
      Exit(True);
    end;
  MachineNext := False;
end;

procedure MachineDumpTape(Machine: TMachine);
var
  Cell : TSymbol;
begin
  for Cell in Machine.Tape do
  begin
    Write(Cell, ' ');
  end;
  WriteLn;
end;

procedure MachineDebugDump(Machine: TMachine);
var
  I, J : LongWord;
begin
  WriteLn('STATE: ', Machine.State);
  MachineDumpTape(Machine);
  for I := 0 to High(Machine.Tape) do
  begin
    if I = Machine.Head then Write('^');
    for J := 0 to High(Machine.Tape[I]) do
      Write(' ');
    if I <> Machine.Head then Write(' ');
  end;
end;

function Tokenize(const Source: AnsiString): TStringDynArray;
var
  Tokens : TStringDynArray = ();
  Words : TStringDynArray;
  Word : AnsiString;
  I : LongWord;
begin
  Words := SplitString(Source, ' ');
  for I := 0 to High(Words) do
  begin
    Word := Trim(Words[I]);
    if Length(Word) > 0 then
      Insert(Word, Tokens, Length(Tokens));
  end;
  Tokenize := Tokens;
end;

function ParseTurp(const FilePath: AnsiString; const Line: LongWord; const Source: AnsiString): TTurp;
const
  CURRENT : Word = 0;
  READ    : Word = 1;
  WRITE   : Word = 2;
  STEP    : Word = 3;
  NEXT    : Word = 4;
var
  Tokens : TStringDynArray;
begin
  Tokens := Tokenize(Source);
  if Length(Tokens) <> 5 then
  begin
    WriteLn(StdErr, Format('%s:%d: ERROR: A single turp is expected to have 5 tokens', [FilePath, Line]));
    Halt(1);
  end;

  ParseTurp.Current := UpCase(Tokens[CURRENT]);
  ParseTurp.Read := Tokens[READ];
  ParseTurp.Write := Tokens[WRITE];
  case Tokens[STEP] of
    'L': ParseTurp.Step := Left;
    'R': ParseTurp.Step := Right;
  else
    WriteLn(StdErr, Format('%s:%d: ERROR: "%s" is not a correct step. Expected "L" or "R"', [FilePath, Line, Tokens[STEP]]));
  end;
  ParseTurp.Next := UpCase(Tokens[NEXT]);
end;

function ReadLines(const FilePath: AnsiString): TStringDynArray;
var
  FileHandle : Text;
  Result : TStringDynArray = ();
  Line : AnsiString;
begin
  Assign(FileHandle, FilePath);
  {$I-}
  Reset(FileHandle);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn(StdErr, 'ERROR: Could not open file "', FilePath, '"');
    Halt(1);
  end;

  while not Eof(FileHandle) do
  begin
    ReadLn(FileHandle, Line);
    Insert(Line, Result, Length(Result));
  end;

  Close(FileHandle);
  ReadLines := Result;
end;

function ReadTokens(const FilePath: AnsiString): TStringDynArray;
var
  Tokens : TStringDynArray = ();
  Lines : TStringDynArray;
  Line : AnsiString;
  LineTokens : TStringDynArray;
  I, J : LongWord;
begin
  Lines := ReadLines(FilePath);
  for I := 0 to High(Lines) do
  begin
    Line := Trim(Lines[I]);
    if Length(Line) > 0 then
    begin
      LineTokens := Tokenize(Line);
      for J := 0 to High(LineTokens) do
        Insert(LineTokens[J], Tokens, Length(Tokens));
    end;
  end;
  ReadTokens := Tokens;
end;

procedure Usage(var Out: Text);
begin
  WriteLn(Out, 'Usage: turp [OPTIONS] <input.turp> <input.tape>');
  WriteLn(Out, 'OPTIONS:');
  WriteLn(Out, '  --help|-h             Print this help to stdout and exit with 0 exit code');
  WriteLn(Out, '  --state|-s [STATE]    Start from a specific initial state (default: BEGIN)');
  WriteLn(Out, '  --head|-p [POSITION]  Start from a specific head position (default: 0)');
  WriteLn(Out, '  --non-interactively   Execute the program non-interactively');
end;

var
  ArgsIndex : LongWord;
  NonInteractively : Boolean = False;
  Positionals : TStringDynArray = ();

  procedure ExpectArgument;
  begin
    if ArgsIndex > ParamCount then
    begin
      Usage(StdErr);
      WriteLn(StdErr, 'ERROR: No argument provided for flag `', ParamStr(ArgsIndex - 1), '`');
      Halt(1);
    end;
  end;

var
  TurpFilePath : AnsiString;
  TapeFilePath : AnsiString;

  I : LongWord;
  TurpLines : TStringDynArray;
  TurpLine : AnsiString;

  Turps : array of TTurp = ();
  Machine : TMachine;
begin
  Machine.Head := 0;
  Machine.State := 'BEGIN';

  ArgsIndex := 1;
  while ArgsIndex <= ParamCount do
  begin
    case ParamStr(ArgsIndex) of
      '--help', '-h':
      begin
        Usage(StdOut);
        Halt;
      end;
      '--state', '-s':
      begin
        Inc(ArgsIndex);
        ExpectArgument;
        Machine.State := ParamStr(ArgsIndex);
      end;
      '--head', '-p':
      begin
        Inc(ArgsIndex);
        ExpectArgument;
        Machine.Head := StrToInt(ParamStr(ArgsIndex));
      end;
      '--non-interactively':
        NonInteractively := True;
    else
      if ParamStr(ArgsIndex)[1] = '-' then
      begin
        Usage(StdErr);
        WriteLn(StdErr, 'ERROR: Unknown flag `', ParamStr(ArgsIndex), '`');
        Halt(1);
      end
      else
        Insert(ParamStr(ArgsIndex), Positionals, High(Positionals));
    end;
    Inc(ArgsIndex);
  end;

  if Length(Positionals) < 2 then
  begin
    Usage(StdErr);
    WriteLn(StdErr, 'ERROR: Missing required positional arguments');
    Halt(1);
  end;

  TurpFilePath := Positionals[1];
  TapeFilePath := Positionals[0];

  TurpLines := ReadLines(TurpFilePath);
  for I := 0 to High(TurpLines) do
  begin
    TurpLine := Trim(TurpLines[I]);
    if (Length(TurpLine) > 0) and (TurpLine[Low(TurpLine)] <> '!') then
      Insert(ParseTurp(TurpFilePath, I + 1, TurpLine), Turps, Length(Turps));
  end;

  Machine.Tape := ReadTokens(TapeFilePath);

  if NonInteractively then
  begin
    while MachineNext(Machine, Turps) do begin end;
    MachineDumpTape(Machine);
  end
  else
    repeat
      MachineDebugDump(Machine);
      ReadLn(Input);
    until not MachineNext(Machine, Turps);
end.
