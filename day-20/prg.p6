class Point2D {
	has $.x;
	has $.y;

	method Str() {
		return "$!x,$!y";
	}
}

class Point3D {
	has $.x;
	has $.y;
	has $.d;

	method Str() {
		return "$!x,$!y`$!d";
	}
}

sub toPoint3D($point, $d) {
	return Point3D.new(:x($point.x), :y($point.y), :d($d));
}

class Maze {
	has @.map;
	has %.portals;

	method isOuter($point) {
		my $outer = $point.x == 2 || $point.y == 2 || $point.x == @!map[0].elems - 3 || $point.y == @!map.elems - 3;
#		say "$point $outer";
		return $outer;
	}
}

sub putPortal(%portals, $portal, $x, $y) {
	my $source;
	my $target;
	if %portals{$portal ~ 1}:exists {
		$source = $portal ~ 2;
		$target = $portal ~ 1;
	} else {
		$source = $portal ~ 1;
		$target = $portal ~ 2;
	}
	%portals{$source} = Point2D.new(:x($x), :y($y));
	return $target;
}

sub readMaze($fileName) {
	my $content = $fileName.IO.slurp;
	my @map = $content.split("\n", :skip-empty).map(-> $l { $l.split("", :skip-empty).Array }).Array;
	my %portals;

	for @map.kv -> $y, @row {
		next if $y == 0 || $y == @map.elems - 1;

		for @row.kv -> $x, $c {
			next if $x == 0 || $x == @row.elems - 1;
			next unless $c ~~ /^\w$/;

			if @map[$y-1;$x] eq '.' {
				my $portal = $c ~ @map[$y+1;$x];
				@map[$y-1;$x] = putPortal(%portals, $portal, $x, $y-1);
			}
			if @map[$y+1;$x] eq '.' {
				my $portal = @map[$y-1;$x] ~ $c;
				@map[$y+1;$x] = putPortal(%portals, $portal, $x, $y+1);
			}
			if @map[$y;$x-1] eq '.' {
				my $portal = $c ~ @map[$y;$x+1];
				@map[$y;$x-1] = putPortal(%portals, $portal, $x-1, $y);
			}
			if @map[$y;$x+1] eq '.' {
				my $portal = @map[$y;$x-1] ~ $c;
				@map[$y;$x+1] = putPortal(%portals, $portal, $x+1, $y);
			}
		}
	}
	for @map.kv -> $y, @row {
		for @row.kv -> $x, $c {
			if $c ~~ /^\w$/ {
				@map[$y;$x] = ' ';
			}
		}
	}

	return Maze.new(:map(@map), :portals(%portals));
}

sub canGo($maze, $point) {
	return $maze.map[$point.y;$point.x] ~~ /^\w | '.'$/;
}

sub portalTarget($maze, $c, $point, $recursive) {
	my $target = $maze.portals{$c};
	my $dd = 0;
	if $recursive {
		$dd = $maze.isOuter($point) ?? -1 !! +1;
	}
	return toPoint3D($target, $point.d + $dd);
}

sub neighbors($maze, $point, $recursive) {
	my $c = $maze.map[$point.y;$point.x];
	return gather {
		my $n;

		$n = Point3D.new(:x($point.x-1), :y($point.y), :d($point.d));
		take $n if canGo($maze, $n);

		$n = Point3D.new(:x($point.x+1), :y($point.y), :d($point.d));
		take $n if canGo($maze, $n);

		$n = Point3D.new(:x($point.x), :y($point.y-1), :d($point.d));
		take $n if canGo($maze, $n);

		$n = Point3D.new(:x($point.x), :y($point.y+1), :d($point.d));
		take $n if canGo($maze, $n);

		unless $c eq '.' {
			if $maze.portals{$c}:exists {
#				say "Using portal $c";
				my $target = portalTarget($maze, $c, $point, $recursive);
				take $target if $target.d >= 0;
			}
		}
	}
}

sub bfs($maze, $point, $recursive, $target) {
	my @queue = $point;
	my %distances = $point => 0;

	my $prevd = -1;
	while @queue.elems > 0 {
		my $p = @queue.shift();
		my $d = %distances{$p};

		if $prevd != $d {
#			say "Distance $d";
		}
		$prevd = $d;

		if $p eqv $target {
#			say "Found target at distance $d";
			return %distances;
		}

#		say "Processing $p at distance $d";

		my @ns = neighbors($maze, $p, $recursive);
		for @ns -> $n {
			unless %distances{$n}:exists {
				%distances{$n} = $d + 1;
				@queue.push($n);
			}
		}
	}

	return %distances;
}



sub MAIN (Str $fileName) {
	my $maze = readMaze($fileName);

	my $AA = toPoint3D($maze.portals{'AA1'}, 0);
	my $ZZ = toPoint3D($maze.portals{'ZZ1'}, 0);

	say "From $AA to $ZZ";

#	say $maze;
#	say neighbors($maze, $AA);

	my %distances;

	%distances = bfs($maze, $AA, False, $ZZ);
	say "Simple distance: ", %distances{$ZZ};

	%distances = bfs($maze, $AA, True, $ZZ);
	say "Recursive distance: ", %distances{$ZZ};
}

