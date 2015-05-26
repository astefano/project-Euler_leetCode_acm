val months = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
val months2 = List(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

//days are 0 (mon), 1 (tues), ..., 6 (sun)
//tuesday on 1 jan 1901
	var start = 1
	var year = 1901
	var sundays = 0
	while(year < 2001) {

	val monthsc = if (year % 4 == 0) months2 else months

	var nmonth = 0
	while (nmonth < 12) {

	if (start+1 % 7 == 6) sundays += 1 

	start = (monthsc(nmonth) + start)%7
	nmonth = nmonth + 1
	}
	year = year + 1
	}

	sundays 

