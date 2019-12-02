BEGIN {
	part1 = 0;
	part2 = 0;
}	

function f(m) {
	mass = m + 0;
	fuel = (mass - mass % 3) / 3 - 2;
	return fuel > 0 ? fuel : 0;
}

function g(m) {
	sum = f(m);
	fuel_for = sum;

	do {
		fuel = f(fuel_for);
		sum += fuel;
		fuel_for = fuel;
	} while (fuel > 0);

	return sum;	
}

{
	part1 += f($1);
	part2 += g($1);
}

END {
	print part1;
	
	print part2;
}
