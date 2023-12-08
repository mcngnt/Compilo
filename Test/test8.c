
int rnd;
int* grid;
int mem;

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

int print_int(int t, int n)
{
	int k;
	if(t <= 9)
	{
		if(t == 0 && n == 0)
		{
			putc(' ');
		}
		else
		{
			putc('0' + t);
		}
		return n;
	}
	k = print_int(t/10, n+1);
	putc('0' + (t % 10));
	return k;
}


int print_grid(int* p)
{
	int k;
	puts("----------------------\n|");
	while(p < grid + 16)
	{
		if( (p-grid) > 0 && ((p - grid) % 4) == 0)
		{
			putc('|');
			putc('\n');
			puts("|                    |\n");
			putc('|');
		}
		putc(' ');
		k = print_int(*p, 0);
		while(3 - k > 0)
		{
			putc(' ');
			k++;
		}
		p++;
	}
	puts("|\n");
	puts("----------------------\n");
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

int put_sq(int* p, int k)
{
	*p = k;
	return;
}

int spawn_sq(int* p)
{
	int* q;
	q = p + (rnd % 16);
	while(*q > 0)
	{
		gen_rnd();
		q = p + (rnd % 16);
	}
	put_sq(q, 2);
	gen_rnd();
	return;
}

int move_grid(int d)
{
	int nb;
	int a;
	int i;
	int* p;
	int* q;
	int b;

	nb = 8;
	while(nb > 0)
	{
		i = 0;
		while(i < 16)
		{
			q = grid + i;
			p = grid + i + d;
			a = *q;
			if(a > 0 && 0 <= i + d && i + d < 16 && (  d == 4 || d == -4 || ( (d == 1 || d == -1) && (i / 4) == ((i + d)/4) )  ) )
			{
				if(*p == 0)
				{
					*p = a;
					*q = 0;

				}
				else if(*p == a)
				{
					*q = 0;
					*p = a * 2;
				}
			}

			++i;
		}
		nb--;
	}
	
	return 0;
}

int main()
{
	int input;
	grid = "xxxxxxxxxxxxxxxx";
	init_grid(grid);
	print_grid(grid);
	puts("Enter seed : \n");
	rnd = getc();

	spawn_sq(grid);
	spawn_sq(grid);


	clear();
	print_grid(grid);

	while((input = getc()) != 'x')
	{
		if(input == 'z')
		{
			move_grid(-4);
			spawn_sq(grid);
		}
		if(input == 's')
		{
			move_grid(4);
			spawn_sq(grid);
		}
		if(input == 'd')
		{
			move_grid(1);
			spawn_sq(grid);
		}
		if(input == 'q')
		{
			move_grid(-1);
			spawn_sq(grid);
		}
		clear();
		print_grid(grid);
	}


	return;

}