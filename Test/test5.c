int res;

int clear()
{
	puts("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
	return 0;
}

int getwidth(int* maze)
{
	int n;
	n = 1;
	while (*maze && *maze != '\n')
		n++, maze++;
	return n;
}

int printmaze(int* maze)
{
	puts(maze);
	puts("Press e to exit\n");
	return 0;
}

int main()
{
	int ch;
	int x;
	int y;
	int width;
	int* pos;
	int* maze;
        maze =  "**************************************************\n"
		"*.**...*....**............********....************\n"
		"*.**.*...**.**.****.*****..........**..*.....*****\n"
		"*....******....****..*****************.*.***.***-*\n"
		"*.*****.....**********.........*.*..**.*.***.*.*.*\n"
		"*.*****.*******....***.*******.*.**.**.*.***...*.*\n"
		"*.*****.*******.**.....***...*.*.**.**...*******.*\n"
		"*.*****.****....**********.*.***.**.********.....*\n"
		"*.*****......*************.*.....**..*******.*****\n"
		"*.*************.....****...*****.***.*******.*****\n"
		"*...............***......*******.............*****\n"
		"**************************************************\n";
	clear();
	x = 1;
	y = 1;
	width = getwidth(maze);
	res = width;
	pos = maze+y*width+x;
	*pos = 'O';
	printmaze(maze);
	*pos = '.';
	while ((ch = getc()) != 'e') {		
		clear();
		if (ch == 'z' && *(maze + (y-1)*width + x) != '*')
			y--;
		if (ch == 's' && *(maze + (y+1)*width + x) != '*')
			y++;
		if (ch == 'q' && *(maze + y*width + x-1) != '*')
			x--;
		if (ch == 'd' && *(maze + y*width + x+1) != '*')
			x++;
		pos = maze+y*width+x;
		if (*pos == '-')
		{
			*pos = 'O';
			puts(maze);
			puts("You won !\n");
			return 0;
		}
		*pos = 'O';
		printmaze(maze);
		*pos = '.';
	}
	return 0;
}