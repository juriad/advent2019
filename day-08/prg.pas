{$H+}
program prg;

uses
	sysutils;

type
	IMG = array of array of array of integer;
	IM = array of array of integer;

var
	image: IMG;
	flattened: IM;
	layers: integer;

function readInput(fileName: string; width: integer; height: integer; var image: IMG): integer;
var
	inFile: TextFile;
	input: string;
	count, i, l, r, c: integer;
begin
	assign(inFile, fileName);
	reset(inFile);
	readln(inFile, input);
	close(inFile);

	count := Length(input);
	i := 1;
	// writeln(count);

	setLength(image, count div width div height);
	for l := 0 to count div width div height - 1 do
	begin
		setLength(image[l], height);
		for r := 0 to height - 1 do
		begin
			setLength(image[l][r], width);
			for c := 0 to width - 1 do
			begin
				// writeln(input[i]);
				image[l][r][c] := StrToInt(input[i]);
				i := i + 1;
			end
		end
	end;
	readInput := count div width div height;
end;

function countLayer(image: IMG; layer: integer; number: integer): integer;
var
	count, r, c: integer;
begin
	count := 0;
	for r := 0 to length(image[layer]) - 1 do
		for c := 0 to length(image[layer][r]) - 1 do
			if image[layer][r][c] = number then
				count := count + 1;
	countLayer := count;
end;

function task1(image: IMG): integer;
var
	l, count, min, argmin, ones, twos: integer;
begin
	min := MAXINT;
	argmin := -1;

	for l := 0 to length(image) - 1 do
	begin
		count := countLayer(image, l, 0);
		// writeln(count);
		if count < min then
		begin
			min := count;
			argmin := l;
		end;
	end;

	// writeln(argmin);	
	ones := countLayer(image, argmin, 1);
	twos := countLayer(image, argmin, 2);
	// writeln(ones);
	// writeln(twos);
	task1 := ones * twos;
end;

procedure flatten(image: IMG; var flattened: IM);
var 
	l, r, c: integer;
begin
	setLength(flattened, length(image[0]));
	for r := 0 to length(flattened) - 1 do
	begin
		setlength(flattened[r], length(image[0][r]));
		for c := 0 to length(flattened[r]) - 1 do
		begin
			for l := 0 to length(image) do
			begin
				if image[l][r][c] < 2 then
				begin
					flattened[r][c] := image[l][r][c];
					break;
				end;
			end;
		end;		
	end;
end;

function toString(im: IM): string;
var
	r, c: integer;
	s: string;
begin
	s := '';
	for r := 0 to length(im) - 1 do
	begin
		for c := 0 to length(im[r]) - 1 do
		begin
			if im[r][c] = 0 then			
				s := s + ' '
			else
				s := s + '#';
		end;
		s := s + #10;
	end;
	toString := s;
end;

begin
	layers := readInput(ParamStr(1), StrToInt(ParamStr(2)), StrToInt(ParamStr(3)), image);
	writeln(task1(image));
	flatten(image, flattened);
	writeln(toString(flattened));
end.

