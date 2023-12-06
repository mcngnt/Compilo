
int rnd;
int* grid;
int* altgrid;

int gen_rnd()
{
	rnd = ((rnd * 9769) + 431) % 32000;
	return;
}

int clear()
{
	puts("\n\n\n\n\n\n\n\n");
	return;
}

int copy_grid()
{
	int i;
	int* p;
	p = grid;
	i = 0;
	while(i < 16)
	{
		*p = *(altgrid + i);
		++i;
		++p;
	}
	return;
}


int print_grid(int* p)
{
	while(p < grid + 16)
	{
		if( ((p - grid) % 4) == 0)
		{
			putc('\n');
		}
		if(*p == 0)
		{
			putc('o');
		}
		else if(*p == 2)
		{
			putc('2');
		}
		else if(*p == 4)
		{
			putc('4');
		}
		else
		{
			putc('8');
		}
		p++;
	}
	putc('\n');
	return;
}

int init_grid(int* p)
{
	int i;
	i = 0;
	while(i < 16)
	{
		*p = 0;
		p++;
		i++;
	}
	return;
}

int put_sq(int* p)
{
	*p = 2;
	return;
}

int spawn_sq(int* p)
{
	p = p + (rnd % 16);
	put_sq(p);
	gen_rnd();
	return;
}

int move_grid(int d)
{
	int nb;
	int a;
	int i;
	int* p;

	nb = 4;
	while(nb > 0)
	{
		init_grid(altgrid);
		i = 0;
		while(i < 16)
		{
			a = *(grid + i);
			if(a > 0 && 0 <= i + d && i + d < 16 && *(grid + i + d) == 0 && (  d == 4 || d == -4 || ( (d == 1 || d == -1) && (i / 4) == ((i + d)/4) )  ) )
			{
				p = altgrid + i + d;
			}
			else
			{
				p = altgrid + i;
			}
			if(a > 0)
			{
				*p = a;
			}

			++i;
		}
		copy_grid();
		nb--;
	}
	
	return 0;
}

int main()
{
	int input;
	grid = "xxxxxxxxxxxxxxxx";
	altgrid = "xxxxxxxxxxxxxxxx";
	init_grid(grid);
	print_grid(grid);
	puts("Enter seed : \n");
	rnd = getc();

	// spawn_sq(grid);
	// spawn_sq(grid);
	// spawn_sq(grid);
	// spawn_sq(grid);
	// spawn_sq(grid);

	put_sq(grid + 4);
	put_sq(grid + 8);
	put_sq(grid + 12);


	clear();
	print_grid(grid);

	while((input = getc()) != 'x')
	{
		if(input == 'z')
		{
			move_grid(-4);
		}
		if(input == 's')
		{
			move_grid(4);
		}
		if(input == 'd')
		{
			move_grid(1);
		}
		if(input == 'q')
		{
			move_grid(-1);
		}
		clear();
		print_grid(grid);
	}


	return;

}